#include "utils.h"
#include <stdio.h>
#include <stdlib.h>

/* Unpacks the 32-bit machine code instruction given into the correct
 * type within the instruction struct */
Instruction parse_instruction(uint32_t instruction_bits) {
  /* YOUR CODE HERE */
  Instruction instruction;
  // add x9, x20, x21   hex: 01 5A 04 B3, binary = 0000 0001 0101 1010 0000 0100 1011 0011
  // Opcode: 0110011 (0x33) Get the Opcode by &ing 0x1111111, bottom 7 bits
  instruction.opcode = instruction_bits & 0x7F; // Get the last 7 bits (0–6) as the opcode

  switch (instruction.opcode) {
  // R-Type instructions (register)
  case 0x33:
  
    instruction.rtype.rd = (instruction_bits >> 7) & 0x1F;  
    instruction.rtype.funct3 = (instruction_bits >> 12) & 0x07; 
    instruction.rtype.rs1 = (instruction_bits >> 15) & 0x1F; 
    instruction.rtype.rs2 = (instruction_bits >> 20) & 0x1F; 
    instruction.rtype.funct7 = (instruction_bits >> 25) & 0x7F;
	
    break;

  // I-Type instructions (immediate,loads)
  case 0x13:
  case 0x3:
  case 0x73:
  
    instruction.itype.rd = (instruction_bits >> 7) & 0x1F;
    instruction.itype.funct3 = (instruction_bits >> 12) & 0x07;
    instruction.itype.rs1 = (instruction_bits >> 15) & 0x1F;
    instruction.itype.imm = (instruction_bits >> 20) & 0xFFF;
	
    break;

  // S-Type instructions (stores) 
  //immediate field split into two parts
  case 0x23:
  
    instruction.stype.imm5 = (instruction_bits >> 7) & 0x1F;
    instruction.stype.funct3 = (instruction_bits >> 12) & 0x07;
    instruction.stype.rs1 = (instruction_bits >> 15) & 0x1F;
    instruction.stype.rs2 = (instruction_bits >> 20) & 0x1F;
    instruction.stype.imm7 = (instruction_bits >> 25) & 0x7F;
	
    break;

  // SB-Type instructions (branching)
  case 0x63:
  
    instruction.sbtype.imm5 = (instruction_bits >> 7) & 0x1F;
    instruction.sbtype.funct3 = (instruction_bits >> 12) & 0x07;
    instruction.sbtype.rs1 = (instruction_bits >> 15) & 0x1F;
    instruction.sbtype.rs2 = (instruction_bits >> 20) & 0x1F;
    instruction.sbtype.imm7 = (instruction_bits >> 25) & 0x7F;
	
    break;

  // U-Type instructions (upper immediate)
  case 0x37:
  
    instruction.utype.rd = (instruction_bits >> 7) & 0x1F;
    instruction.utype.imm = instruction_bits >> 12; //mean uppuer 20 bits form the imm
	
    break;

  // UJ-Type instructions (jump, link)
  case 0x6F:
  
    instruction.ujtype.rd = (instruction_bits >> 7) & 0x1F;
    instruction.ujtype.imm = (instruction_bits >> 12) & 0xFFFFF;
    break;
	
  #ifndef TESTING
  default:
    exit(EXIT_FAILURE);
  #endif
    break;
  }

  return instruction;
}

/************************Helper functions************************/
/* Here, you will need to implement a few common helper functions, 
 * which you will call in other functions when parsing, printing, 
 * or executing the instructions. */

// Extend an n-bit signed value to a full 32-bit integer
int sign_extend_number(unsigned int field, unsigned int n) {
  if ((field >> (n - 1)) & 1) { 
    return field | (~0u << n);   // Fill with 1s if negative
  }
  else{
    return field & ((1U << n) - 1);  // Keep bits as-is if positive
  }
}

// Calculate branch offset from SB-type instruction encoding
int get_branch_offset(Instruction instruction) {
	
  int imm = 0;
  imm |= ((instruction.sbtype.imm5 >> 1) & 0xF) << 1; //4:1   
  imm |= ((instruction.sbtype.imm7 & 0x3F) << 5);   //10:5        
  imm |= ((instruction.sbtype.imm5 & 0x1) << 11);   //11        
  imm |= ((instruction.sbtype.imm7 >> 6) & 0x1) << 12;  //12    
  return sign_extend_number(imm, 13);        
  
}

// Calculate jump offset from UJ-type instruction encoding
int get_jump_offset(Instruction instruction) {
	
  int imm = 0;
  imm |= (instruction.ujtype.imm & 0xFF) << 12; //19:12    
  imm |= ((instruction.ujtype.imm >> 8) & 0x1) << 11; //11     
  imm |= ((instruction.ujtype.imm >> 9) & 0x3FF) << 1;//10:1      
  imm |= ((instruction.ujtype.imm >> 19) & 0x1) << 20; //20     
  return sign_extend_number(imm, 21);             
  
}

// Combine S-type imm5 and imm7 to compute store offset
int get_store_offset(Instruction instruction) {
	
  int imm = (instruction.stype.imm5 & 0x1F) | ((instruction.stype.imm7 & 0x7F) << 5);        
  return sign_extend_number(imm, 12);            
  
}

/************************Helper functions************************/

void handle_invalid_instruction(Instruction instruction) {
  printf("Invalid Instruction: 0x%08x\n", instruction.bits);
}

void handle_invalid_read(Address address) {
  printf("Bad Read. Address: 0x%08x\n", address);
  exit(-1);
}

void handle_invalid_write(Address address) { 
  printf("Bad Write. Address: 0x%08x\n", address);
  exit(-1);
}