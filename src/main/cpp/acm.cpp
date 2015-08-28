#undef SYNTH_MAIN
#include "synth.cpp"
#include <fstream>
#include <sstream>
#include <cstring>


void   acm_init() {}

void   acm_finish() {}

void   acm_solve(oplist_t &l, const vector<coeff_t> &coeffs,
                 const vector<reg_t> &dests, reg_t src, reg_t (*tmpreg)()) {
    ASSERT(dests.size() == coeffs.size());

    cfset_t problem;
    regmap_t * regs = new regmap_t;

    regs->clear();
    (*regs)[1] = src;

    for(int i = coeffs.size()-1; i >= 0; --i) {
        if(coeffs[i]!=1)
          (*regs)[coeffs[i]] = dests[i];
    }

    /* compute registers for all fundamentals ('reduced' constants) */
    for(int i = 0; i < coeffs.size(); ++i) {
        coeff_t full    = coeffs[i];
        coeff_t reduced = fundamental(cfabs(full));
        if(reduced!=1)
            ADD(problem, reduced);
        if(!CONTAINS(*regs, reduced))
            (*regs)[reduced] = tmpreg();
    }

    if_verbose(1) cerr<<"synthesizing "<<problem.size()<<" unique fundamentals\n";

    create_problem(problem);
    GEN_CODE = true;
    solve(&l, regs, tmpreg);

    for(int i = 0; i < coeffs.size(); ++i) {
        coeff_t full    = coeffs[i];
        reg_t   dest    = dests[i];
        coeff_t reduced = fundamental(cfabs(full));
        if(full == 1)
            l.push_back(op::shl(dest, src, 0, full)); /* move dest <- src */

        else if(full == -1)
            l.push_back(op::neg(dest, src, full)); /* move dest <- -1*src */

        /* last op */
        else if(dest == (*regs)[full]) {
            if(full != reduced) {
                int t = (*regs)[reduced];

                /* see if we already computed the negative of it */
                int found_neg = -1;
                for(int j=0; j<i; ++j) {
                    if(coeffs[j] == -full) {
                        found_neg = j;
                        break;
                    }
                }
                /* if not */
                if(found_neg == -1) {
                    /* this is the first instance */
                    int shr = compute_shr(cfabs(full));
                    if(shr!=0) {
                        reg_t tmp = dest;
                        if(full < 0) {
                            tmp = tmpreg();
                            for(int j = i+1; j < coeffs.size(); ++j) {
                                if(coeffs[j] == -full) {
                                    tmp = dests[j];
                                    break;
                                }
                            }
                        }
                        l.push_back(op::shl(tmp, (*regs)[reduced], shr, cfabs(full)));
                        t = tmp;
                    }
                    /* negate if necessary */
                    if(full<0) l.push_back(op::neg(dest,t, full));
                }
                else /* negative was already computed, just negate */
                    if(full < 0) l.push_back(op::neg(dest,dests[found_neg],full));
            }
        }
        else /* we already computed it */
            l.push_back(op::shl(dest, (*regs)[full], 0, full));
    }
    delete regs;
}


int TMP_REG_NUM = 1;
int DEST_REG_NUM = 1;

reg_t tmpreg() { return TMP_REG_NUM++; }
reg_t destreg() { return DEST_REG_NUM++; }

extern "C" {
    #include <string.h>
int jnaCall(int num, int *consts, int buf_size, char *out_buffer) {
    
    srand(time(0));
    acm_init();
    
    SPEC_ALL_TARGETS.clear();
    GIVEN.clear();
    SPEC_TARGETS.clear();
    COST_0.clear();
    COST_1.clear();
    TMP_REG_NUM = 1;
    DEST_REG_NUM = 1;

    ADD(GIVEN, 1);
    for(int i=0; i < num; ++i)
    {
        int c = consts[i];
        ADD(SPEC_TARGETS, fundamental(cfabs(c)));
        SPEC_ALL_TARGETS.push_back(c);
    }

    ADD(COST_0, 1);
    gen_add_coeffs(COST_0, COST_0, COST_1);
    gen_add_coeffs(COST_0, COST_1, COST_2);
    gen_mul_coeffs(COST_1, COST_1, COST_2);
    
    subtract_set(COST_1, COST_0);
    subtract_set(COST_2, COST_0);
    subtract_set(COST_2, COST_1);
    
    vector<coeff_t> targets;
    vector<reg_t> dests;

    targets = SPEC_ALL_TARGETS;
    dests.resize(targets.size());
    if(targets.size() >= TMP_REG_NUM)
        TMP_REG_NUM = targets.size() + 1;
    int i = 0;
    FORALL_V(targets, t) {
        dests[i] = destreg();
        ++i;
    }
    
    std::ostringstream cout;
    
    oplist_t ops;
    acm_solve(ops, targets, dests, 0, tmpreg);
    
    int opcount[op::OPLAST];
    for(int i=0; i<op::OPLAST; ++i) opcount[i]=0;
    for(oplist_t::iterator i = ops.begin(); i!=ops.end(); ++i)
        ++opcount[(*i)->type];
    // adds/subtracts shifts negations
    cout << "cost: "
    << opcount[op::ADD]+opcount[op::SUB] << " "
    << opcount[op::SHL]+opcount[op::SHR] << " "
    << opcount[op::NEG] << endl;
    cout << "// Depth: " << maxdepth(READY) << "\n";
    cout << "outputs: ";
    for(int i=0; i < targets.size(); ++i) {
        cout << dests[i] << "," << targets[i] << " " ;
    }
    cout << endl;

//    cout << "// ADD=1, SUB=2, SHL=3, SHR=4, CMUL=5, NEG, ADDSHL, SUBSHL, SHLSUB, OPLAST" << endl;
    for(oplist_t::iterator i = ops.begin(); i!=ops.end(); ++i){
        cout << "op: ";
        cout << (*i)->res << " ";
        cout << (*i)->type << " ";
        cout << (*i)->r1 << " ";
        cout << (*i)->r2 << " ";
        cout << (*i)->sh << " ";
        cout << (*i)->c << " ";
        cout << endl;
        
    }
    
    acm_finish();

    
    std::string s = cout.str();
    
//    std::cout << s << endl;
    
    int len = s.length();
    
    if(len > buf_size) {
        std::cout << "buffer too small!" << std::endl;
        return -1;
    } else {
        strncpy(out_buffer, s.c_str(), len);
        return len;
    }
    
    return 0;

}
}

