#include <stdio.h>
#include <stdlib.h>

#include <ir/ir.h>
#include <target/util.h>

static char* TLHSREG[] = {
    "A",
    "B",
    "C",
    "D",
    "BP",
    "SP",
};

static void tlhs_emit_line_2op(const int tlhs_inst_pc, const int current_table, const char* imm, const Inst* inst) {
    if (inst->src.type == REG) {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_REG cpu %s %s)",
                current_table, tlhs_inst_pc, imm, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_IMM cpu %s %d)",
                current_table, tlhs_inst_pc, imm, TLHSREG[inst->dst.reg], inst->src.imm);
    }
}

static void tlhs_emit_line_conditional_jump(const int tlhs_inst_pc, const int current_table, const char* imm, const Inst* inst) {
    if (inst->jmp.type == REG && inst->src.type == REG) {
      emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_REG_REG cpu %d %s %s)",
              current_table, tlhs_inst_pc, imm, inst->jmp.reg, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else if (inst->jmp.type == REG && inst->src.type != REG){
      emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_REG_IMM cpu %d %s %s)",
               current_table, tlhs_inst_pc, imm, inst->jmp.reg, TLHSREG[inst->dst.reg], inst->src.imm);
    } else if (inst->jmp.type != REG && inst->src.type == REG){
      emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_IMM_REG cpu %d %s %d)",
               current_table, tlhs_inst_pc, imm, inst->jmp.imm, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else {
      emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (%s_IMM_IMM cpu %d %s %d)",
               current_table, tlhs_inst_pc, imm, inst->jmp.imm, TLHSREG[inst->dst.reg], inst->src.imm);
    }
}

static void tlhs_emit_inst(const int tlhs_inst_pc, const int current_table, const Inst* inst) {
  switch (inst->op) {
  case MOV:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "MV", inst);
    break;

  case ADD:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "ADD", inst);
    break;

  case SUB:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table,"SUB", inst);
    break;

  case LOAD:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "LOAD", inst);
    break;

  case STORE:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "STORE", inst);
    break;

  case PUTC:
    if (inst->src.type == REG) {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (PUTC_REG cpu %s)", current_table, tlhs_inst_pc, TLHSREG[inst->src.reg]);
    } else {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (PUTC_IMM cpu %d)", current_table, tlhs_inst_pc, inst->src.imm);
    }
    break;

  case GETC:
    emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (GETC cpu %s)", current_table, tlhs_inst_pc, TLHSREG[inst->dst.reg]);
    break;

  case EXIT:
    emit_line("Run%d total_cnt debug %d cpu = cpu", current_table, tlhs_inst_pc);
    break;

  case DUMP:
    emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (NOP cpu) -- FIXME: DUMP", current_table, tlhs_inst_pc);
    break;

  case EQ:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "EQ", inst);
    break;

  case NE:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "NE", inst);
    break;

  case LT:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "LT", inst);
    break;

  case GT:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "GT", inst);
    break;

  case LE:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "LE", inst);
    break;

  case GE:
    tlhs_emit_line_2op(tlhs_inst_pc, current_table, "GE", inst);
    break;

  case JEQ:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JEQ", inst);
    break;

  case JNE:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JNE", inst);
    break;

  case JLT:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JLT", inst);
    break;

  case JGT:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JGT", inst);
    break;

  case JLE:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JLE", inst);
    break;

  case JGE:
    tlhs_emit_line_conditional_jump(tlhs_inst_pc, current_table, "JGE", inst);
    break;

  case JMP:
    if (inst->jmp.type == REG) {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (JUMP_REG cpu %s)", current_table, tlhs_inst_pc, TLHSREG[inst->jmp.reg]);
    } else {
        emit_line("Run%d total_cnt debug %d cpu = Run (total_cnt + 1) debug (JUMP_IMM cpu %d)", current_table, tlhs_inst_pc, inst->jmp.imm);
    }
    break;

  default:
    error("oops");
  }
}

void target_tlhs(Module* module) {
    // memory initialize
    emit_line("type family Init (pc :: Nat) (mem :: Mem) where");
    inc_indent();
    Data* data = module->data;
    for (int mp=0; data; data=data->next, mp++) {
        emit_line("Init %d mem = Init %d (Store mem %d %d)", mp, mp+1, mp, data->v);
    }
    emit_line("Init _ mem = mem");
    dec_indent();
    emit_line("");

    int pc_cnt = 0;
    for (Inst* inst = module->text; inst; inst = inst->next) {
        pc_cnt++;
    }

    // PC2ADDR: absorb difference of elvm and tlhs
    emit_line("type family PC2ADDR (pc :: Nat) where");
    inc_indent();
    int prev_pc = -1;
    int tlhs_pc = 0;
    for (Inst* inst = module->text; inst; inst = inst->next, tlhs_pc++) {
        if (prev_pc != inst->pc) {
            emit_line("PC2ADDR %d = %d", inst->pc, tlhs_pc);
        }
        prev_pc = inst->pc;
    }
    dec_indent();
    emit_line("");

    // inst
    const int table_size = 50;
    int table_index = 0;
    int current_table = -1;
    inc_indent();
    for (Inst* inst = module->text; inst; inst = inst->next) {
        if (table_index%table_size == 0) {
            dec_indent();
            current_table += 1;
            emit_line("\ntype family Run%d (total_cnt :: Nat) (debug :: [Nat]) (pc :: Nat) cpu where", current_table);
            inc_indent();
        }
        tlhs_emit_inst(table_index, current_table, inst);
        table_index += 1;
    }
    dec_indent();

    emit_line("\ntype family Run (total_cnt :: Nat) (debug :: [Nat]) cpu where");
    emit_line(" Run total_cnt debug cpu = RunTable (Div (Get cpu PC) %d) total_cnt debug (Get cpu PC) cpu", table_size);

    emit_line("\ntype family RunTable (table_id :: Nat) (total_cnt :: Nat) (debug :: [Nat]) (pc :: Nat) cpu where");
    inc_indent();
    for (int i=0; i<=current_table; i++) {
        emit_line("RunTable %d total_cnt debug pc cpu = Run%d total_cnt debug pc cpu", i, i);
    }
    dec_indent();
}
