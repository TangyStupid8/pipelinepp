#ifndef __STAGE_HELPERS_H__
#define __STAGE_HELPERS_H__

#include <stdio.h>
#include "utils.h"
#include "pipeline.h"

/// EXECUTE STAGE HELPERS ///

/**
 * input  : idex_reg_t
 * output : uint32_t alu_control signal
 **/

 //alu operation decoder takes input and outputs the proper alu operation
uint32_t gen_alu_control(idex_reg_t idex_reg)
{
  uint32_t alu_control = 0;

  switch(idex_reg.instr.opcode) {
    case 0x33: // R-type
      switch(idex_reg.instr.rtype.funct3) {
        case 0x0: // ADD/SUB/MUL
          if(idex_reg.instr.rtype.funct7 == 0x00) alu_control = 0x0; // ADD
          else if(idex_reg.instr.rtype.funct7 == 0x20) alu_control = 0x1; // SUB
          else if(idex_reg.instr.rtype.funct7 == 0x01) alu_control = 0x2; // MUL
          break;
        case 0x1: // SLL/MULH
          if(idex_reg.instr.rtype.funct7 == 0x00) alu_control = 0x3; // SLL
          else if(idex_reg.instr.rtype.funct7 == 0x01) alu_control = 0x4; // MULH
          break;
        case 0x2: alu_control = 0x5; break; // SLT
        case 0x4: // XOR/DIV
          if(idex_reg.instr.rtype.funct7 == 0x00) alu_control = 0x6; // XOR
          else if(idex_reg.instr.rtype.funct7 == 0x01) alu_control = 0x7; // DIV
          break;
        case 0x5: // SRL/SRA
          if(idex_reg.instr.rtype.funct7 == 0x00) alu_control = 0x8; // SRL
          else if(idex_reg.instr.rtype.funct7 == 0x20) alu_control = 0x9; // SRA
          break;
        case 0x6: // OR/REM
          if(idex_reg.instr.rtype.funct7 == 0x00) alu_control = 0xA; // OR
          else if(idex_reg.instr.rtype.funct7 == 0x01) alu_control = 0xB; // REM
          break;
        case 0x7: alu_control = 0xC; break; // AND
      }
      break;
    case 0x13: // I-type (immediate operations)
      switch(idex_reg.instr.itype.funct3) {
        case 0x0: alu_control = 0x0; break; // ADDI
        case 0x1: alu_control = 0x3; break; // SLLI
        case 0x2: alu_control = 0x5; break; // SLTI
        case 0x3: alu_control = 0xD; break; // SLTIU
        case 0x4: alu_control = 0x6; break; // XORI
        case 0x5: // SRLI/SRAI
          if((idex_reg.instr.itype.imm >> 5) == 0x00) alu_control = 0x8; // SRLI
          else alu_control = 0x9; // SRAI
          break;
        case 0x6: alu_control = 0xA; break; // ORI
        case 0x7: alu_control = 0xC; break; // ANDI
      }
      break;
    case 0x03: // Load
    case 0x23: // Store
      alu_control = 0x0; // ADD for address calculation
      break;
    case 0x63: // Branch
      switch(idex_reg.instr.sbtype.funct3) {
        case 0x0: alu_control = 0x1; break; // BEQ (use SUB)
        case 0x1: alu_control = 0x1; break; // BNE (use SUB)
        case 0x4: alu_control = 0x5; break; // BLT (use SLT)
        case 0x5: alu_control = 0x5; break; // BGE (use SLT)
        case 0x6: alu_control = 0xD; break; // BLTU (use SLTU)
        case 0x7: alu_control = 0xD; break; // BGEU (use SLTU)
      }
      break;
    case 0x6F: // JAL
    case 0x37: // LUI
      alu_control = 0x0; // ADD
      break;
    default:
      alu_control = 0x0;
      break;
  }

  return alu_control;
}

/**
 * input  : alu_inp1, alu_inp2, alu_control
 * output : uint32_t alu_result
 **/
// the calculator for the operations depending on the alu_control calue
uint32_t execute_alu(uint32_t alu_inp1, uint32_t alu_inp2, uint32_t alu_control)
{
  uint32_t result;
  switch(alu_control){
    case 0x0: // ADD
      result = alu_inp1 + alu_inp2;
      break;
    case 0x1: // SUB
      result = alu_inp1 - alu_inp2;
      break;
    case 0x2: // MUL
      result = ((int32_t)alu_inp1) * ((int32_t)alu_inp2);
      break;
    case 0x3: // SLL shift left logical
      result = alu_inp1 << (alu_inp2 & 0x1F);
      break;
    case 0x4: // MULH multiply high
      {
        int64_t temp = ((int64_t)(int32_t)alu_inp1) * ((int64_t)(int32_t)alu_inp2);
        result = (uint32_t)(temp >> 32);
      }
      break;
    case 0x5: // SLT set less than
      result = ((int32_t)alu_inp1 < (int32_t)alu_inp2) ? 1 : 0;
      break;
    case 0x6: // XOR
      result = alu_inp1 ^ alu_inp2;
      break;
    case 0x7: // DIV
      if(alu_inp2 == 0) result = 0xFFFFFFFF;
      else result = (int32_t)alu_inp1 / (int32_t)alu_inp2;
      break;
    case 0x8: // SRL shift right logical
      result = alu_inp1 >> (alu_inp2 & 0x1F);
      break;
    case 0x9: // SRA shift left arithmatic preserves sign bit
      result = (int32_t)alu_inp1 >> (alu_inp2 & 0x1F);
      break;
    case 0xA: // OR
      result = alu_inp1 | alu_inp2;
      break;
    case 0xB: // REM remainder
      if(alu_inp2 == 0) result = alu_inp1;
      else result = (int32_t)alu_inp1 % (int32_t)alu_inp2;
      break;
    case 0xC: // AND
      result = alu_inp1 & alu_inp2;
      break;
    case 0xD: // SLTU set less than unsigned
      result = (alu_inp1 < alu_inp2) ? 1 : 0;
      break;
    default:
      result = 0xBADCAFFE;
      break;
  };
  return result;
}

/// DECODE STAGE HELPERS ///

/**
 * input  : Instruction
 * output : uint32_t immediate value
 **/

 //gets the immedate value from I type operations
uint32_t gen_imm(Instruction instruction)
{
  int imm_val = 0;

  switch(instruction.opcode) {
    case 0x13: // I-type (immediate)
    case 0x03: // I-type (load)
    case 0x73: // I-type (ecall)
      imm_val = sign_extend_number(instruction.itype.imm, 12);
      break;
    case 0x23: // S-type (store)
      {
        uint32_t imm = (instruction.stype.imm5 & 0x1F) | ((instruction.stype.imm7 & 0x7F) << 5);
        imm_val = sign_extend_number(imm, 12);
      }
      break;
    case 0x63: // SB-type (branch)
      imm_val = get_branch_offset(instruction);
      break;
    case 0x37: // U-type (lui)  loads immediate bits to upper 20 bits of registrer
      imm_val = instruction.utype.imm << 12;
      break;
    case 0x6F: // UJ-type (jal)
      imm_val = get_jump_offset(instruction);
      break;
    default: // R-type and undefined
      imm_val = 0;
      break;
  };
  return imm_val;
}

/**
 * generates all the control logic that flows around in the pipeline
 * input  : Instruction
 * output : idex_reg_t with control signals set
 **/

 //determines opcodes based on control values
idex_reg_t gen_control(Instruction instruction)
{
  idex_reg_t idex_reg = {0};

  // Copy instruction
  idex_reg.instr = instruction;

  switch(instruction.opcode) {
    case 0x33: // R-type
      idex_reg.reg_write = true;
      idex_reg.alu_src = false; // Use rs2
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 2; // R-type ALU op
      break;
    case 0x13: // I-type (immediate)
      idex_reg.reg_write = true;
      idex_reg.alu_src = true; // Use immediate
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 3; // I-type ALU op
      break;
    case 0x03: // Load
      idex_reg.reg_write = true;
      idex_reg.alu_src = true; // Use immediate for address
      idex_reg.mem_to_reg = true;
      idex_reg.mem_read = true;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 0; // ADD for address
      break;
    case 0x23: // Store
      idex_reg.reg_write = false;
      idex_reg.alu_src = true; // Use immediate for address
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = true;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 0; // ADD for address
      break;
    case 0x63: // Branch
      idex_reg.reg_write = false;
      idex_reg.alu_src = false; // Use rs2 for comparison
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = true;
      idex_reg.jump = false;
      idex_reg.alu_op = 1; // Branch ALU op
      break;
    case 0x37: // LUI
      idex_reg.reg_write = true;
      idex_reg.alu_src = true; // Use immediate
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 0; // Pass immediate through
      break;
    case 0x6F: // JAL
      idex_reg.reg_write = true;
      idex_reg.alu_src = true;
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = true;
      idex_reg.alu_op = 0; // ADD for PC+4
      break;
    case 0x73: // ECALL
      idex_reg.reg_write = false;
      idex_reg.alu_src = false;
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 0;
      break;
    default: // Undefined
      idex_reg.reg_write = false;
      idex_reg.alu_src = false;
      idex_reg.mem_to_reg = false;
      idex_reg.mem_read = false;
      idex_reg.mem_write = false;
      idex_reg.branch = false;
      idex_reg.jump = false;
      idex_reg.alu_op = 0;
      break;
  }
  return idex_reg;
}

/// MEMORY STAGE HELPERS ///

/**
 * evaluates whether a branch must be taken
 * input  : exmem_reg_t
 * output : bool
 **/
//branch decision logic
bool gen_branch(exmem_reg_t exmem_reg)
{
  if (exmem_reg.instr.opcode != 0x63) return false; // Not a branch

  // The values are stored in alu_result and rs2_data
  uint32_t rs1_data = exmem_reg.alu_result;
  uint32_t rs2_data = exmem_reg.rs2_data;

  switch(exmem_reg.instr.sbtype.funct3) {
    case 0x0: // BEQ equal
      return (rs1_data == rs2_data);
    case 0x1: // BNE not equal
      return (rs1_data != rs2_data);
    case 0x4: // BLT less than
      return ((int32_t)rs1_data < (int32_t)rs2_data);
    case 0x5: // BGE greater than or equal
      return ((int32_t)rs1_data >= (int32_t)rs2_data);
    case 0x6: // BLTU less than unsigned
      return (rs1_data < rs2_data);
    case 0x7: // BGEU greater than or equal unsigned
      return (rs1_data >= rs2_data);
    default:
      return false;
  }
}

/// PIPELINE FEATURES ///

/**
 * Task   : Sets the pipeline wires for the forwarding unit's control signals
 *           based on the pipeline register values.
 * input  : pipeline_regs_t*, pipeline_wires_t*
 * output : None
 *
 * */
void gen_forward(pipeline_regs_t* pregs_p, pipeline_wires_t* pwires_p)
{
  // Initialize all forwarding signals
  pwires_p->forward_a_ex = false;
  pwires_p->forward_b_ex = false;
  pwires_p->forward_data_a_ex = 0;
  pwires_p->forward_data_b_ex = 0;
  pwires_p->forward_a_mem = false;
  pwires_p->forward_b_mem = false;
  pwires_p->forward_data_a_mem = 0;
  pwires_p->forward_data_b_mem = 0;

  idex_reg_t* idex = &pregs_p->idex_preg.out;
  exmem_reg_t* exmem = &pregs_p->exmem_preg.out;
  memwb_reg_t* memwb = &pregs_p->memwb_preg.out;

  // EX Hazard Detection (EX/MEM -> EX forwarding)
  if (exmem->reg_write && exmem->rd != 0) {
    // Forward to rs1
    if (exmem->rd == idex->rs1) {
      pwires_p->forward_a_ex = true;
      pwires_p->forward_data_a_ex = exmem->alu_result;
      printf("[FWD]: Resolving EX hazard on rs1: x%d\n", idex->rs1);
      fwd_exex_counter++;
    }

    // Forward to rs2 (for R-type, branches, and stores)
    if (exmem->rd == idex->rs2 && (!idex->alu_src || idex->mem_write)) {
      pwires_p->forward_b_ex = true;
      pwires_p->forward_data_b_ex = exmem->alu_result;
      printf("[FWD]: Resolving EX hazard on rs2: x%d\n", idex->rs2);
      fwd_exex_counter++;
    }
  }

  // MEM Hazard Detection (MEM/WB -> EX forwarding)
  // Only forward if EX hazard is not already handling it
  if (memwb->reg_write && memwb->rd != 0) {
    // Forward to rs1 if not already forwarded from EX
    if (memwb->rd == idex->rs1 && !pwires_p->forward_a_ex) {
      pwires_p->forward_a_mem = true;
      pwires_p->forward_data_a_mem = memwb->mem_to_reg ? memwb->mem_data : memwb->alu_result;
      printf("[FWD]: Resolving MEM hazard on rs1: x%d\n", idex->rs1);
      fwd_exmem_counter++;
    }

    // Forward to rs2 if not already forwarded from EX
    if (memwb->rd == idex->rs2 && !pwires_p->forward_b_ex && (!idex->alu_src || idex->mem_write)) {
      pwires_p->forward_b_mem = true;
      pwires_p->forward_data_b_mem = memwb->mem_to_reg ? memwb->mem_data : memwb->alu_result;
      printf("[FWD]: Resolving MEM hazard on rs2: x%d\n", idex->rs2);
      fwd_exmem_counter++;
    }
  }
}

void detect_hazard(pipeline_regs_t* pregs_p, pipeline_wires_t* pwires_p, regfile_t* regfile_p)
{
  idex_reg_t* idex = &pregs_p->idex_preg.out;
  ifid_reg_t* ifid = &pregs_p->ifid_preg.out;

  // Extract source registers from IFID instruction based on opcode
  uint32_t ifid_rs1 = 0, ifid_rs2 = 0;
  bool uses_rs1 = false, uses_rs2 = false;

  switch(ifid->instr.opcode) {
    case 0x33: // R-type
      ifid_rs1 = ifid->instr.rtype.rs1;
      ifid_rs2 = ifid->instr.rtype.rs2;
      uses_rs1 = true;
      uses_rs2 = true;
      break;
    case 0x13: // I-type (immediate)
    case 0x03: // I-type (load)
      ifid_rs1 = ifid->instr.itype.rs1;
      uses_rs1 = true;
      uses_rs2 = false;
      break;
    case 0x23: // S-type (store)
      ifid_rs1 = ifid->instr.stype.rs1;
      ifid_rs2 = ifid->instr.stype.rs2;
      uses_rs1 = true;
      uses_rs2 = true;
      break;
    case 0x63: // SB-type (branch)
      ifid_rs1 = ifid->instr.sbtype.rs1;
      ifid_rs2 = ifid->instr.sbtype.rs2;
      uses_rs1 = true;
      uses_rs2 = true;
      break;
    case 0x73: // ECALL
      uses_rs1 = false;
      uses_rs2 = false;
      break;
    default: // U-type, UJ-type, etc.
      uses_rs1 = false;
      uses_rs2 = false;
      break;
  }

  // Load-use hazard detection
  if (idex->mem_read && (idex->rd != 0) &&
      ((uses_rs1 && idex->rd == ifid_rs1) || (uses_rs2 && idex->rd == ifid_rs2))) {

    stall_counter++;
    printf("[HZD]: Stalling and rewriting PC: 0x%08x\n", regfile_p->PC - 8);
    pwires_p->stall = true;
  } else {
    pwires_p->stall = false;
  }
}


///////////////////////////////////////////////////////////////////////////////

/// RESERVED FOR PRINTING REGISTER TRACE AFTER EACH CLOCK CYCLE ///
void print_register_trace(regfile_t* regfile_p)
{
  // print
  for (uint8_t i = 0; i < 8; i++)       // 8 columns
  {
    for (uint8_t j = 0; j < 4; j++)     // of 4 registers each
    {
      printf("r%2d=%08x ", i * 4 + j, regfile_p->R[i * 4 + j]);
    }
    printf("\n");
  }
  printf("\n");
}

#endif // __STAGE_HELPERS_H__
