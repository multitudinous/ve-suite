// cmu_model.h: interface for the cmu_model class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CMU_MODEL_H__BBF6BA7C_81A9_48DC_A93F_5C777DBB5FD9__INCLUDED_)
#define AFX_CMU_MODEL_H__BBF6BA7C_81A9_48DC_A93F_5C777DBB5FD9__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <WINDOWS.H>
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <malloc.h>
#include <eh.h>
/* ----- begin variable array index, size info ----- */
#include "link_io.h"
#include "init_io.h"
#include "cmn_io.h"
#include "igcc_io.h"
#include "coal_io.h"
#include "asu_io.h"
#include "tex_io.h"
#include "wgsr_io.h"
#include "srs_io.h"
#include "selx_io.h"
#include "gts_io.h"
#include "stk_io.h"
#include "cseq_io.h"
#include "sum_io.h"

/* ----- end variable array index, size info ----- */

/* ----- begin module index, function info ----- */
#include "prefixes.h"
#include "configs.h"

#include <string>

void exception_trans_func (unsigned int e, _EXCEPTION_POINTERS *pExp);

/* ----- end module index, function info ----- */

//This is a C++ wrapper of the whole CMU plant 
class cmu_model  
{
public:
	cmu_model();
	virtual ~cmu_model();

	int error;

	void LoadDefaults();
	int Initialization();
	void LogLink(int module_id, float* curlink, float savelink[PREFIX_IDX_MAX][LINK_SZ]);

protected:
	void init_link_tp ();
	void init_vars ();
	void lookup_fns (int idx, prefixfn_name_array_t fnlist);
	void init_runfns ();

	void clear_links (float *links, int sz);
	void copy_links (float *to, float *from, int sz);
	int cmp_links (float *to, float *from, int sz, int *linktp);
	int run_modules (int *runconf, int runconf_sz);
	void get_modoutputs (int *runconf, int runconf_sz);
	void set_modinputs (int *runconf, int runconf_sz, int runconf_id);

public:
	void run_orig_model (int *runconf, int runconf_sz, int runconf_id);
	void run_model_withlink(int *runconf, int runconf_sz, int runconf_id, float* link, int sz);

public:
	float *input_vars[PREFIX_IDX_MAX+1];
	int input_sz[PREFIX_IDX_MAX+1];
	float *calc_vars[PREFIX_IDX_MAX+1];
	int calc_sz[PREFIX_IDX_MAX+1];
	float *output_vars[PREFIX_IDX_MAX+1];
	int output_sz[PREFIX_IDX_MAX+1];

	float *general_io;
	int general_io_sz;
	float link_gen[LINK_SZ];
	int link_tp[LINK_SZ];

	//Init module inputs, calcs, outputs
	float init_inputs[1]; // init has no inputs
	float init_calcs[1]; // init has no calcs
	float init_outputs[1]; // init has no outputs

	//CMN inputs, calcs, outputs
	float cmn_inputs[CMN_SZ_IN];
	float cmn_calcs[CMN_SZ_CALC];
	float cmn_outputs[CMN_SZ_OUT];

	//IGCC inputs, calcs, outputs
	float igcc_inputs[IGCC_SZ_IN];
	float igcc_calcs[IGCC_SZ_CALC];
	float igcc_outputs[IGCC_SZ_OUT];

	//COAL inputs, calcs, outputs
	float coal_inputs[1]; // coal has no inputs
	float coal_calcs[1]; // coal has no calcs
	float coal_outputs[1]; // coal has no outputs

	//ASU inputs, calcs, outputs
	float asu_inputs[ASU_SZ_IN];
	float asu_calcs[ASU_SZ_CALC];
	float asu_outputs[ASU_SZ_OUT];

	//TEX inputs, calcs, outputs
	float tex_inputs[TEX_SZ_IN];
	float tex_calcs[TEX_SZ_CALC];
	float tex_outputs[TEX_SZ_OUT];

	//WGSR inputs, calcs, outputs
	float wgsr_inputs[WGSR_SZ_IN];
	float wgsr_calcs[WGSR_SZ_CALC];
	float wgsr_outputs[WGSR_SZ_OUT];

	//SRS inputs, calcs, outputs
	float srs_inputs[SRS_SZ_IN];
	float srs_calcs[SRS_SZ_CALC];
	float srs_outputs[SRS_SZ_OUT];

	//SELX inputs, calcs, outputs
	float selx_inputs[SELX_SZ_IN];
	float selx_calcs[SELX_SZ_CALC];
	float selx_outputs[SELX_SZ_OUT];

	//GTS inputs, calcs, outputs
	float gts_inputs[GTS_SZ_IN];
	float gts_calcs[GTS_SZ_CALC];
	float gts_outputs[GTS_SZ_OUT];

	//STK inputs, calcs, outputs
	float stk_inputs[1]; // stk has no inputs
	float stk_calcs[1]; // stk has no calcs
	float stk_outputs[STK_SZ_OUT];

	//CSEQ inputs, calcs, outputs
	float cseq_inputs[CSEQ_SZ_IN];
	float cseq_calcs[1]; // cseq has no calcs
	float cseq_outputs[CSEQ_SZ_OUT];

	//SUM inputs, calcs, outputs
	float sum_inputs[1]; // sum has no inputs
	float sum_calcs[1]; // sum has no calcs
	float sum_outputs[SUM_SZ_OUT];

/* ----- end variable arrays ----- */

/* ----- begin function arrays ----- */
	HMODULE model_dll;
	prefixfn_fn_array_t runfns[PREFIX_IDX_MAX+1];
/* ----- end function arrays ----- */

	float linkLog[PREFIX_IDX_MAX][LINK_SZ];
	bool _rei_gasi;
};

#endif // !defined(AFX_CMU_MODEL_H__BBF6BA7C_81A9_48DC_A93F_5C777DBB5FD9__INCLUDED_)
