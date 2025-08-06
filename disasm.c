#include <stdio.h> // for stderr
#include <stdlib.h> // for exit()
#include "types.h"
#include "utils.h"

void print_rtype(char *, Instruction);
void print_itype_except_load(char *, Instruction, int);
void print_load(char *, Instruction);
void print_store(char *, Instruction);
void print_branch(char *, Instruction);
void print_lui(Instruction);
void print_jal(Instruction);
void print_ecall(Instruction);
void write_rtype(Instruction);
void write_itype_except_load(Instruction);
void write_load(Instruction);
void write_store(Instruction);
void write_branch(Instruction);

void decode_instruction(uint32_t instruction_bits) {
    // silently return here, the reason to do this is because the pipeline
    // will be uninitialised for the first 4 cycles and so the call to
    // `parse_instruction` will fail.
    if(instruction_bits == 0)
    {
        printf("\n");
        return;
    }
    Instruction instruction = parse_instruction(instruction_bits);
    switch(instruction.opcode) {
        case 0x33:
            write_rtype(instruction);
            break;
        case 0x13:
            write_itype_except_load(instruction);
            break;
        case 0x3:
            write_load(instruction);
            break;
        case 0x23:
            write_store(instruction);
            break;
        case 0x63:
            write_branch(instruction);
            break;
        case 0x37:
            print_lui(instruction);
            break;
        case 0x6F:
            print_jal(instruction);
            break;
        case 0x73:
            print_ecall(instruction);
            break;
        default: // undefined opcode
            handle_invalid_instruction(instruction);
            break;
    }
}


// Decodes and prints R-type instructions which perform operations between registers
void write_rtype(Instruction instruction) {
    switch (instruction.rtype.funct3) {
        case 0x0:
            switch (instruction.rtype.funct7) {

                case 0x00: print_rtype("add", instruction); break; //add
                case 0x01: print_rtype("mul", instruction); break;  //multiply
                case 0x20: print_rtype("sub", instruction); break; //subtract
                default: handle_invalid_instruction(instruction); break;

            }
            break;
        case 0x1:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("sll", instruction);
			}
            else if (instruction.rtype.funct7 == 0x01){
				print_rtype("mulh", instruction);
			}
            else{
				handle_invalid_instruction(instruction);
			}
            break;
        case 0x2:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("slt", instruction);
			}
            else{
				handle_invalid_instruction(instruction);
			}
            break;
        case 0x4:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("xor", instruction);
			}
            else if (instruction.rtype.funct7 == 0x01){
				print_rtype("div", instruction);
			}
            else{
				handle_invalid_instruction(instruction);
			}
            break;
        case 0x5:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("srl", instruction);
			}
            else if (instruction.rtype.funct7 == 0x20){
				print_rtype("sra", instruction);
			}
            else {
				handle_invalid_instruction(instruction);
			}
            break;
        case 0x6:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("or", instruction);
			}
            else if(instruction.rtype.funct7 == 0x01){
				print_rtype("rem", instruction);
			}
            else{
				handle_invalid_instruction(instruction);
			}
            break;
        case 0x7:
            if (instruction.rtype.funct7 == 0x00){
				print_rtype("and", instruction);
			}
            else {
				handle_invalid_instruction(instruction);
			}
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
}

// Handle I-type instructions like addi, ori, etc. (except load)
void write_itype_except_load(Instruction instruction) {
    switch (instruction.itype.funct3) {
        case 0x0:
            print_itype_except_load("addi", instruction, sign_extend_number(instruction.itype.imm, 12)); // Add immediate
            break;
        case 0x1:
            // Handle shift left logical immediate
            int shift_amount = instruction.itype.imm & 0x1F;  // Only lower 5 bits for shift amount
            print_itype_except_load("slli", instruction, shift_amount);
            break;
        case 0x2:
            print_itype_except_load("slti", instruction, sign_extend_number(instruction.itype.imm, 12));// Set less than
            break;
        case 0x3:
            print_itype_except_load("sltiu", instruction, sign_extend_number(instruction.itype.imm, 12)); // Unsigned
            break;
        case 0x4:
            print_itype_except_load("xori", instruction, sign_extend_number(instruction.itype.imm, 12)); // XOR immediate
            break;
        case 0x5: {
            // Handle shift instructions right
            uint32_t funct7 = instruction.itype.imm >> 5;
            int i = instruction.itype.imm & 0x1F;  // Shift amount
            if (funct7 == 0x00) {
                print_itype_except_load("srli", instruction, i); //logical
            }
			else if (funct7 == 0x20) {
                print_itype_except_load("srai", instruction, i); //arithmetic
            }
			else {
                handle_invalid_instruction(instruction);
            }
            break;
        }
        case 0x6:
            print_itype_except_load("ori", instruction, sign_extend_number(instruction.itype.imm, 12));// OR immediate
            break;
        case 0x7:
            print_itype_except_load("andi", instruction, sign_extend_number(instruction.itype.imm, 12)); // AND immediate
            break;
        default:
            handle_invalid_instruction(instruction);
            break;
    }
}

// Handle load instructions (lb, lh, lw, lbu, lhu)
void write_load(Instruction instruction) {
    switch (instruction.itype.funct3) {

        case 0x0: print_load("lb", instruction); break;
        case 0x1: print_load("lh", instruction); break;
        case 0x2: print_load("lw", instruction); break;
        case 0x4: print_load("lbu", instruction); break;
        case 0x5: print_load("lhu", instruction); break;
        default: handle_invalid_instruction(instruction); break;
    }
}

//h-store instructions (sb, sh, sw)
void write_store(Instruction instruction) {
    switch (instruction.stype.funct3) {

        case 0x0: print_store("sb", instruction); break;
        case 0x1: print_store("sh", instruction); break;
        case 0x2: print_store("sw", instruction); break;
        default: handle_invalid_instruction(instruction); break;
    }
}

//h-branch instructions (beq, bne, blt, bge,bltu, bgeu)
void write_branch(Instruction instruction) {
    switch (instruction.sbtype.funct3) {

        case 0x0: print_branch("beq", instruction); break;
        case 0x1: print_branch("bne", instruction); break;
        case 0x4: print_branch("blt", instruction); break;
        case 0x5: print_branch("bge", instruction); break;
        case 0x6: print_branch("bltu", instruction); break;
        case 0x7: print_branch("bgeu", instruction); break;
        default: handle_invalid_instruction(instruction); break;
    }
}

// These functions print formatted output to the screen for each instruction type
void print_rtype(char *name, Instruction instruction) {
    printf(RTYPE_FORMAT, name, instruction.rtype.rd, instruction.rtype.rs1, instruction.rtype.rs2);
}

void print_itype_except_load(char *name, Instruction instruction, int imm) {
    printf(ITYPE_FORMAT, name, instruction.itype.rd, instruction.itype.rs1, imm);
}

void print_load(char *name, Instruction instruction) {
    printf(MEM_FORMAT, name, instruction.itype.rd, sign_extend_number(instruction.itype.imm, 12), instruction.itype.rs1);
}

     //combining imm parts
void print_store(char *name, Instruction instruction) {
    int imm = (instruction.stype.imm5 & 0x1F) | ((instruction.stype.imm7 & 0x7F) << 5);
    imm = sign_extend_number(imm, 12);
    printf(MEM_FORMAT, name, instruction.stype.rs2, imm, instruction.stype.rs1);
}

void print_branch(char *name, Instruction instruction) {
    int offset = get_branch_offset(instruction);
    printf(BRANCH_FORMAT, name, instruction.sbtype.rs1, instruction.sbtype.rs2, offset);
}

void print_lui(Instruction instruction) {
    printf(LUI_FORMAT, instruction.utype.rd, instruction.utype.imm);
}

void print_jal(Instruction instruction) {
    int offset = get_jump_offset(instruction);
    printf(JAL_FORMAT, instruction.ujtype.rd, offset);
}

void print_ecall(Instruction instruction) {
    printf(ECALL_FORMAT);
}
