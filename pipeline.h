#ifndef __PIPELINE_H__
#define __PIPELINE_H__

#include "config.h"
#include "types.h"
#include "cache.h"
#include <stdbool.h>

// Enable debug prints for milestone 1
#define DEBUG_CYCLE
#define DEBUG_REG_TRACE

///////////////////////////////////////////////////////////////////////////////
/// Functionality
///////////////////////////////////////////////////////////////////////////////

extern simulator_config_t sim_config;
extern uint64_t miss_count;
extern uint64_t hit_count;
extern uint64_t total_cycle_counter; // how many clock ticks have passed
extern uint64_t stall_counter;
extern uint64_t branch_counter; // how many branches were taken
extern uint64_t fwd_exex_counter;
extern uint64_t fwd_exmem_counter;
extern uint64_t mem_access_counter; // how many memory operations performed

///////////////////////////////////////////////////////////////////////////////
/// RISC-V Pipeline Register Types
///////////////////////////////////////////////////////////////////////////////

//fetch to instruct
typedef struct
{
  Instruction instr; // the given parsed insruction
  uint32_t    instr_addr; // where in memory did our instruction come from
}ifid_reg_t;

//isntruct to excedute
typedef struct
{
  Instruction instr;
  uint32_t    instr_addr;
  uint32_t    rs1_data; // register 1 data
  uint32_t    rs2_data; // register 2 data
  uint32_t    imm; // immediate value for I type
  uint32_t    rs1; // register numbers for rs1,rs2
  uint32_t    rs2;
  uint32_t    rd; // destination register number
  // Control signals
  bool        reg_write; // should a register be written too
  bool        mem_read; // read from memory
  bool        mem_write; // write to memory
  bool        branch; // is there a branch
  bool        alu_src; // use the immediate value or rs2 for alu operation
  bool        reg_dst; // which destination register
  bool        mem_to_reg; // write to memory or to alu result
  bool        jump; // is this a jump instruction
  uint32_t    alu_op; // alu opcode
}idex_reg_t;
// excecute to memory
typedef struct
{
  Instruction instr;
  uint32_t    instr_addr;
  uint32_t    alu_result; // result from alu operation
  uint32_t    rs2_data; // data to be stored
  uint32_t    rd; // destination
  bool        branch_taken; // branch taken
  uint32_t    branch_target; // where to jump if branch is taken
  // Control signals
  bool        reg_write;
  bool        mem_read;
  bool        mem_write;
  bool        mem_to_reg;
}exmem_reg_t;

typedef struct
{
  Instruction instr;
  uint32_t    instr_addr;
  uint32_t    alu_result; //result from alu operation
  uint32_t    mem_data; // data read from memory
  uint32_t    rd; // destination register
  // Control signals
  bool        reg_write;
  bool        mem_to_reg;
}memwb_reg_t;


///////////////////////////////////////////////////////////////////////////////
/// Register types with input and output variants for simulator
///////////////////////////////////////////////////////////////////////////////

typedef struct
{
  ifid_reg_t inp;
  ifid_reg_t out;
}ifid_reg_pair_t;

typedef struct
{
  idex_reg_t inp;
  idex_reg_t out;
}idex_reg_pair_t;

typedef struct
{
  exmem_reg_t inp;
  exmem_reg_t out;
}exmem_reg_pair_t;

typedef struct
{
  memwb_reg_t inp;
  memwb_reg_t out;
}memwb_reg_pair_t;

///////////////////////////////////////////////////////////////////////////////
/// Functional pipeline requirements
///////////////////////////////////////////////////////////////////////////////
//all pipeline registers
typedef struct
{
  ifid_reg_pair_t  ifid_preg;
  idex_reg_pair_t  idex_preg;
  exmem_reg_pair_t exmem_preg;
  memwb_reg_pair_t memwb_preg;
}pipeline_regs_t;

//pipeline control wires
typedef struct
{
  bool      pcsrc; // is being branched 1 = branch = 0 PC + 4
  uint32_t  pc_src0; // sequential PC + 4
  uint32_t  pc_src1; // Branch jump target pc
  uint32_t  next_pc; // store next pc
  // Hazard detection and forwarding
  bool      stall; // pipeline stall signal
  bool      forward_a_ex; // forward from EX stage to rs1
  bool      forward_b_ex; // forward from EX stage to rs2
  uint32_t  forward_data_a_ex; // forwarded data for rs1 from EX
  uint32_t  forward_data_b_ex; // forwarded data for rs2 from EX
  bool      forward_a_mem; // forward from MEM stage to rs1
  bool      forward_b_mem; // forward from MEM stage to rs2
  uint32_t  forward_data_a_mem; // forwarded data for rs1 from MEM
  uint32_t  forward_data_b_mem; // forwarded data for rs2 from MEM
}pipeline_wires_t;


///////////////////////////////////////////////////////////////////////////////
/// Function definitions for different stages
///////////////////////////////////////////////////////////////////////////////

/**
 * output : ifid_reg_t
 **/
ifid_reg_t stage_fetch(pipeline_wires_t* pwires_p, regfile_t* regfile_p, Byte* memory_p);

/**
 * output : idex_reg_t
 **/
idex_reg_t stage_decode(ifid_reg_t ifid_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p);

/**
 * output : exmem_reg_t
 **/
exmem_reg_t stage_execute(idex_reg_t idex_reg, pipeline_wires_t* pwires_p);

/**
 * output : memwb_reg_t
 **/
memwb_reg_t stage_mem(exmem_reg_t exmem_reg, pipeline_wires_t* pwires_p, Byte* memory, Cache* cache_p);

/**
 * output : write_data
 **/
void stage_writeback(memwb_reg_t memwb_reg, pipeline_wires_t* pwires_p, regfile_t* regfile_p);

void cycle_pipeline(regfile_t* regfile_p, Byte* memory_p, Cache* cache_p, pipeline_regs_t* pregs_p, pipeline_wires_t* pwires_p, bool* ecall_exit);

void bootstrap(pipeline_wires_t* pwires_p, pipeline_regs_t* pregs_p, regfile_t* regfile_p);

#endif  // __PIPELINE_H__
