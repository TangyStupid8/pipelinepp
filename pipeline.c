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
  pwires_p->pc_src0 = regfile_p->PC;
  pwires_p->pcsrc = false;
  pwires_p->pc_src1 = 0;
  pwires_p->next_pc = 0;
  pwires_p->stall = false;
}

///////////////////////////
/// STAGE FUNCTIONALITY ///
///////////////////////////

ifid_reg_t stage_fetch(pipeline_wires_t* pwires_p, regfile_t* regfile_p, Byte* memory_p)
{
  ifid_reg_t ifid_reg = {0};

  uint32_t instruction_bits = load(memory_p, regfile_p->PC, LENGTH_WORD);
  Instruction instruction = parse_instruction(instruction_bits);
  ifid_reg.instr = instruction;
  ifid_reg.instr_addr = regfile_p->PC;
  ifid_reg.instr.bits = instruction_bits;

  if (!pwires_p->stall) {
    regfile_p->PC += 4;
  }
  pwires_p->pc_src0 = regfile_p->PC;

  #ifdef DEBUG_CYCLE
  printf("[IF ]: Instruction [%08x]@[%08x]: ", instruction_bits, ifid_reg.instr_addr);
  decode_instruction(instruction_bits);
  #endif

  return ifid_reg;
}

idex_reg_t stage_decode(ifid_reg_t ifid_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p)
{
  idex_reg_t idex_reg = {0};
  idex_reg.instr = ifid_reg.instr;
  idex_reg.instr_addr = ifid_reg.instr_addr;
  idex_reg.instr.bits = ifid_reg.instr.bits;

  switch(ifid_reg.instr.opcode) {
    case 0x33:
      idex_reg.rs1 = ifid_reg.instr.rtype.rs1;
      idex_reg.rs2 = ifid_reg.instr.rtype.rs2;
      idex_reg.rd = ifid_reg.instr.rtype.rd;
      break;
    case 0x13:
    case 0x03:
    case 0x73:
      idex_reg.rs1 = ifid_reg.instr.itype.rs1;
      idex_reg.rs2 = 0;
      idex_reg.rd = ifid_reg.instr.itype.rd;
      break;
    case 0x23:
      idex_reg.rs1 = ifid_reg.instr.stype.rs1;
      idex_reg.rs2 = ifid_reg.instr.stype.rs2;
      idex_reg.rd = 0;
      break;
    case 0x63:
      idex_reg.rs1 = ifid_reg.instr.sbtype.rs1;
      idex_reg.rs2 = ifid_reg.instr.sbtype.rs2;
      idex_reg.rd = 0;
      break;
    case 0x37:
    case 0x6F:
      idex_reg.rs1 = 0;
      idex_reg.rs2 = 0;
      idex_reg.rd = ifid_reg.instr.utype.rd;
      break;
    default:
      idex_reg.rs1 = 0;
      idex_reg.rs2 = 0;
      idex_reg.rd = 0;
      break;
  }

  idex_reg.rs1_data = regfile_p->R[idex_reg.rs1];
  idex_reg.rs2_data = regfile_p->R[idex_reg.rs2];
  idex_reg.imm = gen_imm(ifid_reg.instr);

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

exmem_reg_t stage_execute(idex_reg_t idex_reg, pipeline_wires_t* pwires_p)
{
  exmem_reg_t exmem_reg = {0};
  exmem_reg.instr = idex_reg.instr;
  exmem_reg.instr_addr = idex_reg.instr_addr;
  exmem_reg.instr.bits = idex_reg.instr.bits;
  exmem_reg.rd = idex_reg.rd;

  exmem_reg.reg_write = idex_reg.reg_write;
  exmem_reg.mem_read = idex_reg.mem_read;
  exmem_reg.mem_write = idex_reg.mem_write;
  exmem_reg.mem_to_reg = idex_reg.mem_to_reg;

  // ALU input 1 with forwarding
  uint32_t alu_inp1 = idex_reg.rs1_data;
  if (pwires_p->forward_a_ex) {
    alu_inp1 = pwires_p->forward_data_a_ex;
  } else if (pwires_p->forward_a_mem) {
    alu_inp1 = pwires_p->forward_data_a_mem;
  }

  // ALU input 2 with forwarding
  uint32_t alu_inp2;
  if (idex_reg.alu_src) {
    alu_inp2 = idex_reg.imm;
  } else {
    alu_inp2 = idex_reg.rs2_data;
    if (pwires_p->forward_b_ex) {
      alu_inp2 = pwires_p->forward_data_b_ex;
    } else if (pwires_p->forward_b_mem) {
      alu_inp2 = pwires_p->forward_data_b_mem;
    }
  }

  // Handle rs2_data for store instructions with forwarding
  exmem_reg.rs2_data = idex_reg.rs2_data;
  if (pwires_p->forward_b_ex) {
    exmem_reg.rs2_data = pwires_p->forward_data_b_ex;
  } else if (pwires_p->forward_b_mem) {
    exmem_reg.rs2_data = pwires_p->forward_data_b_mem;
  }

  // Special cases
  if (idex_reg.instr.opcode == 0x6F) {
    alu_inp1 = idex_reg.instr_addr;
    alu_inp2 = 4;
  } else if (idex_reg.instr.opcode == 0x37) {
    alu_inp1 = 0;
    alu_inp2 = idex_reg.imm;
  }

  uint32_t alu_control = gen_alu_control(idex_reg);
  exmem_reg.alu_result = execute_alu(alu_inp1, alu_inp2, alu_control);

  exmem_reg.branch_taken = false;
  exmem_reg.branch_target = idex_reg.instr_addr + idex_reg.imm;

  // For branch instructions, store the CORRECTLY forwarded rs1 and rs2 values
  if (idex_reg.instr.opcode == 0x63) {
    exmem_reg.alu_result = alu_inp1;  // Store forwarded rs1 value
    // rs2_data is already set correctly above with forwarding
  }

  #ifdef DEBUG_CYCLE
  printf("[EX ]: Instruction [%08x]@[%08x]: ", idex_reg.instr.bits, idex_reg.instr_addr);
  decode_instruction(idex_reg.instr.bits);
  #endif

  return exmem_reg;
}

memwb_reg_t stage_mem(exmem_reg_t exmem_reg, pipeline_wires_t* pwires_p, Byte* memory_p, Cache* cache_p)
{
  memwb_reg_t memwb_reg = {0};
  memwb_reg.instr = exmem_reg.instr;
  memwb_reg.instr_addr = exmem_reg.instr_addr;
  memwb_reg.instr.bits = exmem_reg.instr.bits;
  memwb_reg.rd = exmem_reg.rd;
  memwb_reg.alu_result = exmem_reg.alu_result;
  memwb_reg.reg_write = exmem_reg.reg_write;
  memwb_reg.mem_to_reg = exmem_reg.mem_to_reg;

  if (exmem_reg.mem_read) {
    switch(exmem_reg.instr.itype.funct3) {
      case 0x0:
        memwb_reg.mem_data = sign_extend_number(load(memory_p, exmem_reg.alu_result, LENGTH_BYTE), 8);
        break;
      case 0x1:
        memwb_reg.mem_data = sign_extend_number(load(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD), 16);
        break;
      case 0x2:
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_WORD);
        break;
      case 0x4:
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_BYTE);
        break;
      case 0x5:
        memwb_reg.mem_data = load(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD);
        break;
    }
  } else if (exmem_reg.mem_write) {
    switch(exmem_reg.instr.stype.funct3) {
      case 0x0:
        store(memory_p, exmem_reg.alu_result, LENGTH_BYTE, exmem_reg.rs2_data);
        break;
      case 0x1:
        store(memory_p, exmem_reg.alu_result, LENGTH_HALF_WORD, exmem_reg.rs2_data);
        break;
      case 0x2:
        store(memory_p, exmem_reg.alu_result, LENGTH_WORD, exmem_reg.rs2_data);
        break;
    }
  }

  if (exmem_reg.instr.opcode == 0x63 && gen_branch(exmem_reg)) {
    pwires_p->pcsrc = true;
    pwires_p->pc_src1 = exmem_reg.branch_target;
    pwires_p->next_pc = 1;
    branch_counter++;

  }

  if (exmem_reg.instr.opcode == 0x6F) {
    pwires_p->pcsrc = true;
    pwires_p->pc_src1 = exmem_reg.branch_target;
    pwires_p->next_pc = 1;
    branch_counter++;
  }

  #ifdef DEBUG_CYCLE
  printf("[MEM]: Instruction [%08x]@[%08x]: ", exmem_reg.instr.bits, exmem_reg.instr_addr);
  decode_instruction(exmem_reg.instr.bits);
  #endif

  return memwb_reg;
}

void stage_writeback(memwb_reg_t memwb_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p)
{
  #ifdef DEBUG_CYCLE
  printf("[WB ]: Instruction [%08x]@[%08x]: ", memwb_reg.instr.bits, memwb_reg.instr_addr);
  decode_instruction(memwb_reg.instr.bits);
  #endif

  if (memwb_reg.reg_write && memwb_reg.rd != 0) {
    regfile_p->R[memwb_reg.rd] = memwb_reg.mem_to_reg ? memwb_reg.mem_data : memwb_reg.alu_result;
  }

  regfile_p->R[0] = 0;
}

void cycle_pipeline(regfile_t* regfile_p, Byte* memory_p, Cache* cache_p, pipeline_regs_t* pregs_p, pipeline_wires_t* pwires_p, bool* ecall_exit)
{
#ifdef DEBUG_CYCLE
  printf("v==============");
  printf("Cycle Counter = %5ld", total_cycle_counter);
  printf("==============v\n\n");
#endif

  // Handle PC updates and pipeline flushes from previous cycle
  if (pwires_p->pcsrc) {
    regfile_p->PC = pwires_p->pc_src1;
    pwires_p->pcsrc = false;
  }

  // Detect load-use hazards BEFORE fetch (using current pipeline state)
  detect_hazard(pregs_p, pwires_p, regfile_p);

if (pwires_p->stall) {
  // Keep IFID the same (no new fetch)
  pregs_p->ifid_preg.inp = pregs_p->ifid_preg.out;

  // Insert NOP into IDEX with correct address (PC-8 from the stalled instruction)
  pregs_p->idex_preg.inp = (idex_reg_t){
    .instr = { .opcode = 0x13 },
    .instr.bits = 0x00000013,
    .instr_addr = regfile_p->PC - 8  // Address of the instruction being repeated
  };
} else {
  pregs_p->ifid_preg.inp = stage_fetch(pwires_p, regfile_p, memory_p);
  pregs_p->idex_preg.inp = stage_decode(pregs_p->ifid_preg.out, pwires_p, regfile_p);
}


  gen_forward(pregs_p, pwires_p);
  pregs_p->exmem_preg.inp = stage_execute(pregs_p->idex_preg.out, pwires_p);
  pregs_p->memwb_preg.inp = stage_mem(pregs_p->exmem_preg.out, pwires_p, memory_p, cache_p);
  stage_writeback(pregs_p->memwb_preg.out, pwires_p, regfile_p);

  // Handle pipeline flush AFTER all stages have executed
  if (pwires_p->next_pc == 1) {
    printf("[CPL]: Pipeline Flushed\n");

    // Clear IFID
    pregs_p->ifid_preg.inp = (ifid_reg_t){
      .instr.bits = 0x00000013,
      .instr_addr = pregs_p->ifid_preg.inp.instr_addr
    };

    // Clear IDEX
    pregs_p->idex_preg.inp = (idex_reg_t){
      .instr.bits = 0x00000013,
      .instr_addr = pregs_p->idex_preg.inp.instr_addr
    };

    // Clear EXMEM
    pregs_p->exmem_preg.inp = (exmem_reg_t){
      .instr.bits = 0x00000013,
      .instr_addr = pregs_p->exmem_preg.inp.instr_addr
    };

    pwires_p->next_pc = 0;
  }

  // Update pipeline registers
pregs_p->ifid_preg.out = pregs_p->ifid_preg.inp;
pregs_p->idex_preg.out = pregs_p->idex_preg.inp;
pregs_p->exmem_preg.out = pregs_p->exmem_preg.inp;
pregs_p->memwb_preg.out = pregs_p->memwb_preg.inp;

  total_cycle_counter++;

#ifdef DEBUG_REG_TRACE
  print_register_trace(regfile_p);
#endif

  if ((pregs_p->memwb_preg.out.instr.bits == 0x00000073) && (regfile_p->R[10] == 10)) {
    *(ecall_exit) = true;
  }
}
