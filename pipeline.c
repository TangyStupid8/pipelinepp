#include <stdbool.h>
#include "cache.h"
#include "riscv.h"
#include "types.h"
#include "utils.h"
#include "pipeline.h"
#include "stage_helpers.h"

uint64_t total_cycle_counter = 0;
uint64_t miss_count = 0;
uint64_t hit_count = 0;
uint64_t stall_counter = 0;
uint64_t branch_counter = 0;
uint64_t fwd_exex_counter = 0;
uint64_t fwd_exmem_counter = 0;
uint64_t mem_access_counter = 0;

simulator_config_t sim_config = {0};

///////////////////////////////////////////////////////////////////////////////

//starting the processor
void bootstrap(pipeline_wires_t* pwires_p, pipeline_regs_t* pregs_p, regfile_t* regfile_p)
{

  pwires_p->pc_src0 = regfile_p->PC; // set to current pc path
  pwires_p->pcsrc = false; // pc soruce currently is sequentual PC + 4
  pwires_p->pc_src1 = 0; // no branch/ jump targets yet
}

///////////////////////////
/// STAGE FUNCTIONALITY ///
///////////////////////////

/**
 * STAGE  : stage_fetch
 * output : ifid_reg_t
 **/
ifid_reg_t stage_fetch(pipeline_wires_t* pwires_p, regfile_t* regfile_p, Byte* memory_p)
{
  ifid_reg_t ifid_reg = {0};

  // Fetch instruction from memory at PC
  uint32_t instruction_bits = load(memory_p, regfile_p->PC, LENGTH_WORD);

  // Parse the instruction
  // turn the instruction bits into a proper instruction
  Instruction instruction = parse_instruction(instruction_bits);

  // Fill the IF/ID register
  // saves the insruction and its address
  ifid_reg.instr = instruction;
  ifid_reg.instr_addr = regfile_p->PC;
  ifid_reg.instr.bits = instruction_bits;

  // Update PC for next instruction
  regfile_p->PC += 4;
  pwires_p->pc_src0 = regfile_p->PC;

  #ifdef DEBUG_CYCLE
  printf("[IF ]: Instruction [%08x]@[%08x]: ", instruction_bits, ifid_reg.instr_addr);
  decode_instruction(instruction_bits);
  #endif

  return ifid_reg;
}

/**
 * STAGE  : stage_decode
 * output : idex_reg_t
 **/
idex_reg_t stage_decode(ifid_reg_t ifid_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p)
{
  idex_reg_t idex_reg = {0};

  // Copy instruction info
  //get instruction info from ifid
  idex_reg.instr = ifid_reg.instr;
  idex_reg.instr_addr = ifid_reg.instr_addr;
  idex_reg.instr.bits = ifid_reg.instr.bits;

  // find out which instruction register to use
  switch(ifid_reg.instr.opcode) {
    case 0x33: // R-type
      idex_reg.rs1 = ifid_reg.instr.rtype.rs1;
      idex_reg.rs2 = ifid_reg.instr.rtype.rs2;
      idex_reg.rd = ifid_reg.instr.rtype.rd;
      break;
    case 0x13: // I-type immediate
    case 0x03: // I-type load
    case 0x73: // I-type ecall
      idex_reg.rs1 = ifid_reg.instr.itype.rs1;
      idex_reg.rs2 = 0;
      idex_reg.rd = ifid_reg.instr.itype.rd;
      break;
    case 0x23: // S-type store
      idex_reg.rs1 = ifid_reg.instr.stype.rs1;
      idex_reg.rs2 = ifid_reg.instr.stype.rs2;
      idex_reg.rd = 0;
      break;
    case 0x63: // SB-type branch
      idex_reg.rs1 = ifid_reg.instr.sbtype.rs1;
      idex_reg.rs2 = ifid_reg.instr.sbtype.rs2;
      idex_reg.rd = 0;
      break;
    case 0x37: // U-type lui
    case 0x6F: // UJ-type jal
      idex_reg.rs1 = 0;
      idex_reg.rs2 = 0;
      idex_reg.rd = ifid_reg.instr.utype.rd; // Same position for U and UJ
      break;
    default:
      idex_reg.rs1 = 0;
      idex_reg.rs2 = 0;
      idex_reg.rd = 0;
      break;
  }

  // Read register file
  // get the actual data from the associated register
  //what is rs1 and rs2
  idex_reg.rs1_data = regfile_p->R[idex_reg.rs1];
  idex_reg.rs2_data = regfile_p->R[idex_reg.rs2];

  // get the immediate value for I type
  idex_reg.imm = gen_imm(ifid_reg.instr);

  // Generate control signals
  idex_reg_t control_signals = gen_control(ifid_reg.instr);
  idex_reg.reg_write = control_signals.reg_write;
  idex_reg.mem_read = control_signals.mem_read;
  idex_reg.mem_write = control_signals.mem_write;
  idex_reg.branch = control_signals.branch;
  idex_reg.alu_src = control_signals.alu_src;
  idex_reg.reg_dst = control_signals.reg_dst;
  idex_reg.mem_to_reg = control_signals.mem_to_reg;
  idex_reg.jump = control_signals.jump;
  idex_reg.alu_op = control_signals.alu_op;

  #ifdef DEBUG_CYCLE
  printf("[ID ]: Instruction [%08x]@[%08x]: ", ifid_reg.instr.bits, ifid_reg.instr_addr);
  decode_instruction(ifid_reg.instr.bits);
  #endif

  return idex_reg;
}

/**
 * STAGE  : stage_execute
 * output : exmem_reg_t
 **/
exmem_reg_t stage_execute(idex_reg_t idex_reg, pipeline_wires_t* pwires_p)
{
  exmem_reg_t exmem_reg = {0};

  // Copy instruction info that is going to be executed
  exmem_reg.instr = idex_reg.instr;
  exmem_reg.instr_addr = idex_reg.instr_addr;
  exmem_reg.instr.bits = idex_reg.instr.bits;
  exmem_reg.rd = idex_reg.rd;

  // Copy control signals
  exmem_reg.reg_write = idex_reg.reg_write;
  exmem_reg.mem_read = idex_reg.mem_read;
  exmem_reg.mem_write = idex_reg.mem_write;
  exmem_reg.mem_to_reg = idex_reg.mem_to_reg;

  // ALU operation
  // the add multiply div, etc
  uint32_t alu_inp1 = idex_reg.rs1_data;
  uint32_t alu_inp2;
  if(idex_reg.alu_src){
    alu_inp2 = idex_reg.imm;
  }
  else{
     alu_inp2 = idex_reg.rs2_data;
  }

  // Special case for JAL - calculate PC + 4 for return address
  if (idex_reg.instr.opcode == 0x6F) {
    alu_inp1 = idex_reg.instr_addr;
    alu_inp2 = 4;
  }
  // Special case for LUI - pass immediate directly
  else if (idex_reg.instr.opcode == 0x37) {
    alu_inp1 = 0;
    alu_inp2 = idex_reg.imm;
  }
   // perform the actual operation
  uint32_t alu_control = gen_alu_control(idex_reg);
  exmem_reg.alu_result = execute_alu(alu_inp1, alu_inp2, alu_control);
  exmem_reg.rs2_data = idex_reg.rs2_data;

  // if there is a branch calculate the target
  exmem_reg.branch_taken = false;
  exmem_reg.branch_target = idex_reg.instr_addr + idex_reg.imm;

  // For branch instructions, we need to pass the register values for comparison
  // get the data required for the branch
  if (idex_reg.instr.opcode == 0x63) { // Branch instruction opcode
    // Store the register values for gen_branch to use
    exmem_reg.alu_result = idex_reg.rs1_data;  // Store rs1 value in alu_result
    exmem_reg.rs2_data = idex_reg.rs2_data;    // rs2_data already stored??
  }

  #ifdef DEBUG_CYCLE
  printf("[EX ]: Instruction [%08x]@[%08x]: ", idex_reg.instr.bits, idex_reg.instr_addr);
  decode_instruction(idex_reg.instr.bits);
  #endif

  return exmem_reg;
}

/**
 * STAGE  : stage_mem
 * output : memwb_reg_t
 **/
memwb_reg_t stage_mem(exmem_reg_t exmem_reg, pipeline_wires_t* pwires_p, Byte* memory_p, Cache* cache_p)
{
  memwb_reg_t memwb_reg = {0};

  // Copy instruction info
  memwb_reg.instr = exmem_reg.instr;
  memwb_reg.instr_addr = exmem_reg.instr_addr;
  memwb_reg.instr.bits = exmem_reg.instr.bits;
  memwb_reg.rd = exmem_reg.rd;
  memwb_reg.alu_result = exmem_reg.alu_result;

  // Copy control signals
  memwb_reg.reg_write = exmem_reg.reg_write;
  memwb_reg.mem_to_reg = exmem_reg.mem_to_reg;

  // Memory operations
  if (exmem_reg.mem_read) {
    // Load operation: reading data from memory
    switch(exmem_reg.instr.itype.funct3) { // different size of data
      case 0x0: // LB
        memwb_reg.mem_data = sign_extend_number(load(memory_p, exmem_reg.alu_result, LENGTH_BYTE), 8);
        break;
      case 0x1: // LH
        memwb_reg.mem_data = sign_extend_number(load(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD), 16);
        break;
      case 0x2: // LW
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_WORD);
        break;
      case 0x4: // LBU
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_BYTE);
        break;
      case 0x5: // LHU
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD);
        break;
    }
  } else if (exmem_reg.mem_write) {
    // Store operation: write data to memory
    switch(exmem_reg.instr.stype.funct3) { // writing the different sizes of data
      case 0x0: // SB
        store(memory_p, exmem_reg.alu_result, LENGTH_BYTE, exmem_reg.rs2_data);
        break;
      case 0x1: // SH
        store(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD, exmem_reg.rs2_data);
        break;
      case 0x2: // SW
        store(memory_p, exmem_reg.alu_result, LENGTH_WORD, exmem_reg.rs2_data);
        break;
    }
  }

  // Handle branch logic??
  if (exmem_reg.instr.opcode == 0x63) { // Branch instruction
    bool should_branch = gen_branch(exmem_reg); // checks the register if it should branch
    if (should_branch) {
      // Set pipeline wires to indicate branch taken
      // a branch has occured and move the pc to the designated location
      pwires_p->pcsrc = true;
      pwires_p->pc_src1 = exmem_reg.branch_target;
    }
  }

  // Handle JAL logic??
  if (exmem_reg.instr.opcode == 0x6F) { // JAL instruction
    // JAL always jumps
    pwires_p->pcsrc = true;
    pwires_p->pc_src1 = exmem_reg.branch_target;
  }

  #ifdef DEBUG_CYCLE
  printf("[MEM]: Instruction [%08x]@[%08x]: ", exmem_reg.instr.bits, exmem_reg.instr_addr);
  decode_instruction(exmem_reg.instr.bits);
  #endif

  return memwb_reg;
}

/**
 * STAGE  : stage_writeback
 * output : nothing - The state of the register file may be changed
 **/
void stage_writeback(memwb_reg_t memwb_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p)
{
  #ifdef DEBUG_CYCLE
  printf("[WB ]: Instruction [%08x]@[%08x]: ", memwb_reg.instr.bits, memwb_reg.instr_addr);
  decode_instruction(memwb_reg.instr.bits);
  #endif

  // Write back to register file - ALWAYS ensure r0 stays 0
  if (memwb_reg.reg_write && memwb_reg.rd != 0) {
    if (memwb_reg.mem_to_reg) {
      regfile_p->R[memwb_reg.rd] = memwb_reg.mem_data; // write to memory
    } else {
      regfile_p->R[memwb_reg.rd] = memwb_reg.alu_result; // write to ALU result
    }
  }

  // Ensure r0 is always 0
  regfile_p->R[0] = 0;
}

///////////////////////////////////////////////////////////////////////////////

/**
 * excite the pipeline with one clock cycle
 **/
void cycle_pipeline(regfile_t* regfile_p, Byte* memory_p, Cache* cache_p, pipeline_regs_t* pregs_p, pipeline_wires_t* pwires_p, bool* ecall_exit)
{
  #ifdef DEBUG_CYCLE
  printf("v==============");
  printf("Cycle Counter = %5ld", total_cycle_counter);
  printf("==============v\n\n");
  #endif

  // process each stage

  /* Output               |    Stage      |       Inputs  */
  pregs_p->ifid_preg.inp  = stage_fetch     (pwires_p, regfile_p, memory_p);

  pregs_p->idex_preg.inp  = stage_decode    (pregs_p->ifid_preg.out, pwires_p, regfile_p);

  pregs_p->exmem_preg.inp = stage_execute   (pregs_p->idex_preg.out, pwires_p);

  pregs_p->memwb_preg.inp = stage_mem       (pregs_p->exmem_preg.out, pwires_p, memory_p, cache_p);

                            stage_writeback (pregs_p->memwb_preg.out, pwires_p, regfile_p);

  // Handle PC control for branches/jumps
  if (pwires_p->pcsrc) {
    regfile_p->PC = pwires_p->pc_src1;
    pwires_p->pc_src0 = regfile_p->PC;
    pwires_p->pcsrc = false; // Reset for next cycle
  }

  // update all the output registers for the next cycle from the input registers in the current cycle
  pregs_p->ifid_preg.out  = pregs_p->ifid_preg.inp;
  pregs_p->idex_preg.out  = pregs_p->idex_preg.inp;
  pregs_p->exmem_preg.out = pregs_p->exmem_preg.inp;
  pregs_p->memwb_preg.out = pregs_p->memwb_preg.inp;

  /////////////////// NO CHANGES BELOW THIS ARE REQUIRED //////////////////////

  // increment the cycle
  total_cycle_counter++;

  #ifdef DEBUG_REG_TRACE
  print_register_trace(regfile_p);
  #endif

  /**
   * check ecall condition
   * To do this, the value stored in R[10] (a0 or x10) should be 10.
   * Hence, the ecall condition is checked by the existence of following
   * two instructions in sequence:
   * 1. <instr>  x10, <val1>, <val2>
   * 2. ecall
   *
   * The first instruction must write the value 10 to x10.
   * The second instruction is the ecall (opcode: 0x73)
   *
   * The condition checks whether the R[10] value is 10 when the
   * `memwb_reg.instr.opcode` == 0x73 (to propagate the ecall)
   *
   * If more functionality on ecall needs to be added, it can be done
   * by adding more conditions on the value of R[10]
   */
  if( (pregs_p->memwb_preg.out.instr.bits == 0x00000073) &&
      (regfile_p->R[10] == 10) )
  {
    *(ecall_exit) = true;
  }
}
