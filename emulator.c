#include <stdio.h> // for stderr
#include <stdlib.h> // for exit()
#include "types.h"
#include "utils.h"
#include "riscv.h"

void execute_rtype(Instruction, Processor *);
void execute_itype_except_load(Instruction, Processor *);
void execute_branch(Instruction, Processor *);
void execute_jal(Instruction, Processor *);
void execute_load(Instruction, Processor *, Byte *);
void execute_store(Instruction, Processor *, Byte *);
void execute_ecall(Processor *, Byte *);
void execute_lui(Instruction, Processor *);

void execute_instruction(uint32_t instruction_bits, Processor *processor,Byte *memory) {    
    Instruction instruction = parse_instruction(instruction_bits);
    switch(instruction.opcode) {
        case 0x33:
            execute_rtype(instruction, processor);
            break;
        case 0x13:
            execute_itype_except_load(instruction, processor);
            break;
        case 0x73:
            execute_ecall(processor, memory);
            break;
        case 0x63:
            execute_branch(instruction, processor);
            break;
        case 0x6F:
            execute_jal(instruction, processor);
            break;
        case 0x23:
            execute_store(instruction, processor, memory);
            break;
        case 0x03:
            execute_load(instruction, processor, memory);
            break;
        case 0x37:
            execute_lui(instruction, processor);
            break;
        default: // undefined opcode
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

void execute_rtype(Instruction instruction, Processor *processor) {
    switch (instruction.rtype.funct3){
        case 0x0:// op code for add sub and mul
            switch (instruction.rtype.funct7) { // funct7 will differentiate the choice of add sub or mul
                case 0x0:
                    // Add
                    processor->R[instruction.rtype.rd] = ((sWord)processor->R[instruction.rtype.rs1]) + ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x1:
                    // Mul
                    processor->R[instruction.rtype.rd] = ((sWord)processor->R[instruction.rtype.rs1]) * ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                case 0x20:
                    // Sub
                    processor->R[instruction.rtype.rd] = ((sWord)processor->R[instruction.rtype.rs1]) - ((sWord)processor->R[instruction.rtype.rs2]);
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x1:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // SLL - Shift Left Logical
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] << (processor->R[instruction.rtype.rs2] & 0x1F);// makes it so u can only shift by 0-31 any more is pointless and will cause errors
                    break;
                case 0x1:
                    // MULH - Multiply High
                    {
                        long long result = ((long long)(sWord)processor->R[instruction.rtype.rs1]) * ((long long)(sWord)processor->R[instruction.rtype.rs2]); // same as multiply but casts to ensure a 64 bit result 
                        processor->R[instruction.rtype.rd] = (Word)(result >> 32);// only stores the upper 32 half since we are only storing 32 bits
                    }
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x2:
            if (instruction.rtype.funct7 == 0x0) { // returns 1 or 0 depending if 1 is less than 0
                // SLT - Set Less Than
                if(((sWord)processor->R[instruction.rtype.rs1] < (sWord)processor->R[instruction.rtype.rs2])){
					processor->R[instruction.rtype.rd] = 1;
				}
				else{
					processor->R[instruction.rtype.rd] = 0;
				}
            } 
			else{
                handle_invalid_instruction(instruction);
                exit(-1);
            }
            break;
        case 0x4:
            switch (instruction.rtype.funct7) { // funct7 opcode to determine xor or div
                case 0x0: // simply returns the xor
                    // XOR
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] ^ processor->R[instruction.rtype.rs2];
                    break;
                case 0x1:
                    // DIV
                    if (processor->R[instruction.rtype.rs2] == 0) { // checks if the divison is by 0 first if so return -1 
                        processor->R[instruction.rtype.rd] = -1; // Division by zero
                    } 
					else{ // if it is not divided by 0 proceed
                        processor->R[instruction.rtype.rd] = (sWord)processor->R[instruction.rtype.rs1] / (sWord)processor->R[instruction.rtype.rs2];
                    }
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x5:
            switch (instruction.rtype.funct7) { // fucnt7 opcode to dtermine shift right or shift right arithmatic
                case 0x0:
                    // SRL - Shift Right Logical
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] >> (processor->R[instruction.rtype.rs2] & 0x1F); // makes the shift only 0-31
                    break;
                case 0x20:
                    // SRA - Shift Right Arithmetic
                    processor->R[instruction.rtype.rd] = (sWord)processor->R[instruction.rtype.rs1] >> (processor->R[instruction.rtype.rs2] & 0x1F); // casts as sWord to preserve the signbit by copying the signbit over instead of just adding a 0
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x6:
            switch (instruction.rtype.funct7) {
                case 0x0:
                    // OR
                    processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] | processor->R[instruction.rtype.rs2]; //Simple Or 
                    break;
                case 0x1:
                    // REM - Remainder
                    if (processor->R[instruction.rtype.rs2] == 0) { // ignores it since if its modulus by 0 its the value itself
                        processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1]; // Remainder with zero divisor
                    } else { // if it is not 0 modulus them
                        processor->R[instruction.rtype.rd] = (sWord)processor->R[instruction.rtype.rs1] % (sWord)processor->R[instruction.rtype.rs2];
                    }
                    break;
                default:
                    handle_invalid_instruction(instruction);
                    exit(-1);
                    break;
            }
            break;
        case 0x7:
            if (instruction.rtype.funct7 == 0x0) {
                // AND
                processor->R[instruction.rtype.rd] = processor->R[instruction.rtype.rs1] & processor->R[instruction.rtype.rs2]; // simple logical and
            } else {
                handle_invalid_instruction(instruction);
                exit(-1);
            }
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    // update PC moves to the next instruction which is 4 bytes away
    processor->PC += 4;
}

void execute_itype_except_load(Instruction instruction, Processor *processor) { // imm value is just a constant that apart of the value itself so deosnt need to get the value from another spot in memory
    switch (instruction.itype.funct3) {
        case 0x0:
            // ADDI - Add Immediate
            processor->R[instruction.itype.rd] = (sWord)processor->R[instruction.itype.rs1] + sign_extend_number(instruction.itype.imm, 12); // turns the immediate constant in a 32 bit while maintaining the sign bit and adds it to the rs1
            break;
        case 0x1:
            // SLLI - Shift Left Logical Immediate
            {
                int i = instruction.itype.imm & 0x1F; // holds the intermediate lower 5 bit values in 32 bits since u can only shift 0-31
                processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] << i; // shifts it by above value
            }
            break;
        case 0x2:
            // SLTI - Set Less Than Immediate
			if(((sWord)processor->R[instruction.itype.rs1] < sign_extend_number(instruction.itype.imm, 12))){
				processor->R[instruction.itype.rd] = 1;
			}
			else{
				processor->R[instruction.itype.rd] = 0;
			}
            break;
        case 0x3:
            // SLTIU - Set Less Than Immediate Unsigned			
			if((processor->R[instruction.itype.rs1] < (Word)sign_extend_number(instruction.itype.imm, 12))){
				processor->R[instruction.itype.rd] = 1;
			}
			else{
				processor->R[instruction.itype.rd] = 0;
			}
            break;
        case 0x4:
            // XORI - XOR Immediate
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] ^ sign_extend_number(instruction.itype.imm, 12); // xor the immediate value scaled up to 32 bits
            break;
        case 0x5:
            // SRLI and SRAI - Shift Right Logical/Arithmetic Immediate
            {
                uint32_t funct7 = instruction.itype.imm >> 5; // uses the upper 5 bits acting like opcode to determine a right shift or left shift
                int i = instruction.itype.imm & 0x1F; // hold the bottom 5 bits for shifting 0-31
                if (funct7 == 0x0) {
                    // SRLI
                    processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] >> i; // shifts right
                } else if (funct7 == 0x20) {
                    // SRAI
                    processor->R[instruction.itype.rd] = (sWord)processor->R[instruction.itype.rs1] >> i; // shifts right arithmatically preserving sign bit
                } else {
                    handle_invalid_instruction(instruction);
                }
            }
            break;
        case 0x6:
            // ORI - OR Immediate
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] | sign_extend_number(instruction.itype.imm, 12); // simple or
            break;
        case 0x7:
            // ANDI - AND Immediate
            processor->R[instruction.itype.rd] = processor->R[instruction.itype.rs1] & sign_extend_number(instruction.itype.imm, 12); // simple and
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
    processor->PC += 4;
}

void execute_ecall(Processor *p, Byte *memory) { // impliments the system calls
    Register i;
    
    // syscall number is given by a0 (x10) p->R[10]
    // argument is given by a1 p->R[11]
    switch(p->R[10]) {
        case 1: // print an integer stored in register a1
            printf("%d",p->R[11]);
            p->PC += 4;
            break;
        case 4: // print a string points to the address of where the string is stored
            for(i=p->R[11];i<MEMORY_SPACE && load(memory,i,LENGTH_BYTE);i++) { // prints if its less than the memory space 1MB or if the value itself is there in memory
                printf("%c",load(memory,i,LENGTH_BYTE)); // print the value in memory 
            }
            p->PC += 4;
            break;
        case 10: // exit
            printf("exiting the simulator\n");
            exit(0);
            break;
        case 11: // print a character in register a1
            printf("%c",p->R[11]);
            p->PC += 4;
            break;
        default: // undefined ecall
            printf("Illegal ecall number %d\n", p->R[10]);
            exit(-1);
            break;
    }
}
//conditional jump instruction
void execute_branch(Instruction instruction, Processor *processor) { // allows the processor to go to a different location based on a conditions the ones stated below increasing efficiency in cases by skipping uncessesary instructions
    int offset = get_branch_offset(instruction); // turns all the immediate bit from the instruction type into a single 32 bit value
    
    switch (instruction.sbtype.funct3) {
        case 0x0:
            // BEQ - Branch if Equal
            if (processor->R[instruction.sbtype.rs1] == processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        case 0x1:
            // BNE - Branch if Not Equal
            if (processor->R[instruction.sbtype.rs1] != processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        case 0x4:
            // BLT - Branch if Less Than
            if ((sWord)processor->R[instruction.sbtype.rs1] < (sWord)processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        case 0x5:
            // BGE - Branch if Greater or Equal
            if ((sWord)processor->R[instruction.sbtype.rs1] >= (sWord)processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        case 0x6:
            // BLTU - Branch if Less Than Unsigned
            if (processor->R[instruction.sbtype.rs1] < processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        case 0x7:
            // BGEU - Branch if Greater or Equal Unsigned
            if (processor->R[instruction.sbtype.rs1] >= processor->R[instruction.sbtype.rs2]) {
                processor->PC += offset;
            } else {
                processor->PC += 4;
            }
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
}

void execute_load(Instruction instruction, Processor *processor, Byte *memory) {
    int offset = sign_extend_number(instruction.itype.imm, 12); // calculates the memory address itno the offset
    Address address = processor->R[instruction.itype.rs1] + offset;
    // loads the data based on the data type
    switch (instruction.itype.funct3) {
        case 0x0:
            // LB - Load Byte
            processor->R[instruction.itype.rd] = sign_extend_number(load(memory, address, LENGTH_BYTE), 8);
            break;
        case 0x1:
            // LH - Load Halfword
            processor->R[instruction.itype.rd] = sign_extend_number(load(memory, address, LENGTH_HALF_WORD), 16);
            break;
        case 0x2:
            // LW - Load Word
            processor->R[instruction.itype.rd] = load(memory, address, LENGTH_WORD);
            break;
        case 0x4:
            // LBU - Load Byte Unsigned
            processor->R[instruction.itype.rd] = load(memory, address, LENGTH_BYTE);
            break;
        case 0x5:
            // LHU - Load Halfword Unsigned
            processor->R[instruction.itype.rd] = load(memory, address, LENGTH_HALF_WORD);
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
    processor->PC += 4;
}

void execute_store(Instruction instruction, Processor *processor, Byte *memory) {
    int offset = get_store_offset(instruction); //combines the immediate values instrction specifically for a S-type
    Address address = processor->R[instruction.stype.rs1] + offset; //offsets the base address to find the next spot where it should be stored in memory
    Word value = processor->R[instruction.stype.rs2]; //gets the value that needs to be stored
    //using the opcode to determine which value is being stored
    switch (instruction.stype.funct3) {
        case 0x0:
            // SB - Store Byte
            store(memory, address, LENGTH_BYTE, value);
            break;
        case 0x1:
            // SH - Store Halfword
            store(memory, address, LENGTH_HALF_WORD, value);
            break;
        case 0x2:
            // SW - Store Word
            store(memory, address, LENGTH_WORD, value);
            break;
        default:
            handle_invalid_instruction(instruction);
            exit(-1);
            break;
    }
    processor->PC += 4;
}

void execute_jal(Instruction instruction, Processor *processor) { // jump and saves the return address
    int offset = get_jump_offset(instruction); // convert instruction to 32 bits and extend the sign bit //combines the immediate values instrction specifically for a UJ-type
    
    // Store return address (PC + 4) in destination register
    processor->R[instruction.ujtype.rd] = processor->PC + 4;
    
    // Jump to PC + offset
    processor->PC += offset;
}

void execute_lui(Instruction instruction, Processor *processor) {
    // LUI - Load Upper Immediate
    // Load 20-bit immediate into upper 20 bits of destination register, lower 12 bits are zeroed
    processor->R[instruction.utype.rd] = instruction.utype.imm << 12;
    
    // Update PC
    processor->PC += 4;
}

void store(Byte *memory, Address address, Alignment alignment, Word value) { //allignemtn is how many bytes to store
    if (alignment == LENGTH_BYTE) { //store 1 byte
        memory[address] = value & 0xFF;
    } else if (alignment == LENGTH_HALF_WORD) { // stores 2 bytes 
        memory[address] = value & 0xFF;
        memory[address + 1] = (value >> 8) & 0xFF;  // shifts right by 1 byte to store the 2nd byte
    } else if (alignment == LENGTH_WORD) { // stores 4 bytes 
        memory[address] = value & 0xFF;
        memory[address + 1] = (value >> 8) & 0xFF;
        memory[address + 2] = (value >> 16) & 0xFF;
        memory[address + 3] = (value >> 24) & 0xFF;
    } else {
        printf("Error: Unrecognized alignment %d\n", alignment);
        exit(-1);
    }
}

Word load(Byte *memory, Address address, Alignment alignment) {
    if(alignment == LENGTH_BYTE) {
        return memory[address];
    } else if(alignment == LENGTH_HALF_WORD) {
        return (memory[address+1] << 8) + memory[address];
    } else if(alignment == LENGTH_WORD) {
        return (memory[address+3] << 24) + (memory[address+2] << 16)
               + (memory[address+1] << 8) + memory[address];
    } else {
        printf("Error: Unrecognized alignment %d\n", alignment);
        exit(-1);
    }
}