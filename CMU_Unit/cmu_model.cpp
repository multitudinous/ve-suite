// cmu_model.cpp: implementation of the cmu_model class.
//
//////////////////////////////////////////////////////////////////////

#include "cmu_model.h"
#include "iostream"
/* tex_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:04.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t TEX_PREFIXFNS = {
	"model.dll",
	"TEX_mp_TEX_INIT",
	"TEX_mp_TEX_SETINPUTS",
	"TEX_mp_TEX_SETLINKS",
	"TEX_mp_TEX_GETLINKS",
	"TEX_mp_TEX_GETCALC",
	"TEX_mp_TEX_GETRESULTS",
	""
};

/* wgsr_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:05.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t WGSR_PREFIXFNS = {
	"model.dll",
	"WGSR_mp_WGSR_INIT",
	"WGSR_mp_WGSR_SETINPUTS",
	"WGSR_mp_WGSR_SETLINKS",
	"WGSR_mp_WGSR_GETLINKS",
	"WGSR_mp_WGSR_GETCALC",
	"WGSR_mp_WGSR_GETRESULTS",
	""
};

/* stk_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:04.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t STK_PREFIXFNS = {
	"model.dll",
	"STK_mp_STK_INIT",
	"STK_mp_STK_SETINPUTS",
	"STK_mp_STK_SETLINKS",
	"STK_mp_STK_GETLINKS",
	"STK_mp_STK_GETCALC",
	"STK_mp_STK_GETRESULTS",
	""
};

/* srs_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:04.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t SRS_PREFIXFNS = {
	"model.dll",
	"SRS_mp_SRS_INIT",
	"SRS_mp_SRS_SETINPUTS",
	"SRS_mp_SRS_SETLINKS",
	"SRS_mp_SRS_GETLINKS",
	"SRS_mp_SRS_GETCALC",
	"SRS_mp_SRS_GETRESULTS",
	""
};

/* selx_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:04.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t SELX_PREFIXFNS = {
	"model.dll",
	"SELX_mp_SELX_INIT",
	"SELX_mp_SELX_SETINPUTS",
	"SELX_mp_SELX_SETLINKS",
	"SELX_mp_SELX_GETLINKS",
	"SELX_mp_SELX_GETCALC",
	"SELX_mp_SELX_GETRESULTS",
	""
};

/* init_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/14/03 13:11:31.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t INIT_PREFIXFNS = {
	"model.dll",
	"INIT_mp_INIT_INIT",
	"INIT_mp_INIT_SETINPUTS",
	"INIT_mp_INIT_SETLINKS",
	"INIT_mp_INIT_GETLINKS",
	"INIT_mp_INIT_GETCALC",
	"INIT_mp_INIT_GETRESULTS",
	""
};

/* igcc_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:03.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t IGCC_PREFIXFNS = {
	"model.dll",
	"IGCC_mp_IGCC_INIT",
	"IGCC_mp_IGCC_SETINPUTS",
	"IGCC_mp_IGCC_SETLINKS",
	"IGCC_mp_IGCC_GETLINKS",
	"IGCC_mp_IGCC_GETCALC",
	"IGCC_mp_IGCC_GETRESULTS",
	""
};

/* gts_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:03.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t GTS_PREFIXFNS = {
	"model.dll",
	"GTS_mp_GTS_INIT",
	"GTS_mp_GTS_SETINPUTS",
	"GTS_mp_GTS_SETLINKS",
	"GTS_mp_GTS_GETLINKS",
	"GTS_mp_GTS_GETCALC",
	"GTS_mp_GTS_GETRESULTS",
	""
};

/* cseq_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:03.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t CSEQ_PREFIXFNS = {
	"model.dll",
	"CSEQ_mp_CSEQ_INIT",
	"CSEQ_mp_CSEQ_SETINPUTS",
	"CSEQ_mp_CSEQ_SETLINKS",
	"CSEQ_mp_CSEQ_GETLINKS",
	"CSEQ_mp_CSEQ_GETCALC",
	"CSEQ_mp_CSEQ_GETRESULTS",
	""
};

/* coal_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:03.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t COAL_PREFIXFNS = {
	"model.dll",
	"COAL_mp_COAL_INIT",
	"COAL_mp_COAL_SETINPUTS",
	"COAL_mp_COAL_SETLINKS",
	"COAL_mp_COAL_GETLINKS",
	"COAL_mp_COAL_GETCALC",
	"COAL_mp_COAL_GETRESULTS",
	""
};

/* cmn_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/14/03 13:11:29.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t CMN_PREFIXFNS = {
	"model.dll",
	"CMN_mp_CMN_INIT",
	"CMN_mp_CMN_SETINPUTS",
	"CMN_mp_CMN_SETLINKS",
	"CMN_mp_CMN_GETLINKS",
	"CMN_mp_CMN_GETCALC",
	"CMN_mp_CMN_GETRESULTS",
	"CMN_mp_CMN_SETCONFIG"
};

/* asu_fns.c -- module function names */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/15/03 00:05:04.
/* Version IECM Interface 4.0*/

/* This file requires prefixes.h. */

prefixfn_name_array_t ASU_PREFIXFNS = {
	"model.dll",
	"ASU_mp_ASU_INIT",
	"ASU_mp_ASU_SETINPUTS",
	"ASU_mp_ASU_SETLINKS",
	"ASU_mp_ASU_GETLINKS",
	"ASU_mp_ASU_GETCALC",
	"ASU_mp_ASU_GETRESULTS",
	""
};

prefixfn_name_array_t SUM_PREFIXFNS = {
	"model.dll",
	"SUM_mp_SUM_INIT",
	"SUM_mp_SUM_SETINPUTS",
	"SUM_mp_SUM_SETLINKS",
	"SUM_mp_SUM_GETLINKS",
	"SUM_mp_SUM_GETCALC",
	"SUM_mp_SUM_GETRESULTS",
	""
};


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

cmu_model::cmu_model()
{

	general_io = NULL;
	general_io_sz = 0;
	
	init_inputs[0] = 0.0f ; // init has no inputs
	
	coal_inputs[0] = 0.0f; // coal has no inputs

	stk_inputs[0] = 0.0f; // stk has no inputs

	sum_inputs[0] = 0.0f; // sum has no inputs

	model_dll=NULL;
	
	error=0;
}


cmu_model::~cmu_model()
{
  if (model_dll != NULL) FreeLibrary (model_dll);
  if (general_io != NULL) free (general_io);

}

void cmu_model::LoadDefaults()
{

float dft_cmn_inputs[CMN_SZ_IN] = /* 132 */
  {
    2.0f, 30.0f, 45.0f, 35.0f, 0.0f, 10.0f, 9.0f, 12.0f, 0.0f, 8.5f,
    4.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    2000.0f, 1.0f, 2.0f, 25.0f, 13260.0f, 73.81f, 4.88f, 5.41f, 0.06f, 2.13f,
    1.42f, 7.24f, 5.05f, 51.00f, 30.00f, 5.60f, 4.20f, 0.76f, 1.40f, 0.40f,
    1.50f, 1.80f, 2.60f, 0.74f, 0.0f, 1.0f, 1.0f, 24.82f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 14.7f, 77.0f, 75.0f, 0.0f,
    1.0f, 1.0f, 2.0f, 2.0f, 19430.0f, 87.18f, 12.5f, 0.0f, 0.0f, 0.3f,
    0.02f, 0.0f, 0.0f, 2.0f, 22764.0f, 74.829f, 25.171f, 0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f
  };


float dft_igcc_inputs[IGCC_SZ_IN] = /* 34 */
  {
    0.018f, 0.0f, 95.0f, 9.0f, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 90.0f, 0.0f, 1.0f,
    25.0f, 2.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f
  };

float dft_coal_inputs[1] = { 0.0f }; // coal has no inputs

float dft_asu_inputs[ASU_SZ_IN] = /* 35 */
  {
    0.0f, 1.0f, 1.0f, 1998.0f, 4.0f, 15.0f, 10.0f, 15.0f, 5.0f, 0.5f,
    1.0f, 1.0f, 2.0f, 0.5f, 1.0f, 100.0f, 6.67f, 4.75f, 2.0f, 40.0f,
    30.0f, 95.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, 1.0f, 1.0f
  };

float dft_tex_inputs[TEX_SZ_IN] = /* 133 */
  {
    3.0f, 0.0f, 1.0f, 1.0f, 0.5f, 0.5061f, 2450.0f, 615.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1998.0f, 0.0f, 1.0f, 1.0f, 1.0f,
    4.0f, 15.0f, 10.0f, 15.0f, 0.0f, 1.0f, 1.0f, 0.5f, 1.0f, 1.0f,
    2.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 100.0f, 0.0f, 1.0f, 1.0f,
    6.67f, 4.75f, 0.0f, 1.0f, 1.0f, 40.0f, 30.0f, 0.0f, 1.0f, 1.0f,
	0.0f, 1.0f, 1.0f
  };

float dft_wgsr_inputs[WGSR_SZ_IN] = /* 51 */
  {
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1998.0f, 4.0f,
    15.0f, 10.0f, 15.0f, 5.0f, 0.5f, 1.0f, 1.0f, 2.0f, 0.5f, 1.0f,
    1.0f, 1.0f, 100.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1.0f,
    4.75f, 0.0f, 1.0f, 1.0f, 40.0f, 30.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    0.0f
  };

float dft_srs_inputs[SRS_SZ_IN] = /* 78 */
  {
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 95.0f, 99.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1998.0f,
    4.0f, 15.0f, 10.0f, 15.0f, 0.0f, 1.0f, 1.0f, 0.5f, 1.0f, 1.0f,
    2.0f, 0.5f, 1.0f, 1.0f, 1.0f, 100.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 10.0f, 90.0f,
    6.67f, 4.75f, 0.0f, 1.0f, 1.0f, 40.0f, 30.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 25000.0f, 0.0f, 1.0f, 1.0f, 10000.0f
  };

float dft_selx_inputs[SELX_SZ_IN] = /* 77 */
  {
    0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1998.0f, 4.0f, 15.0f, 10.0f,
    15.0f, 10.0f, 0.5f, 1.0f, 1.0f, 2.0f, 0.5f, 1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 100.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f, 1.0f, 60.0f, 2.0f, 4.75f, 0.0f, 1.0f, 1.0f,
    40.0f, 30.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f,
    1.0f, 3.0f, 0.0f, 1.0f, 1.0f, 100.0f, 0.0f, 1.0f, 1.0f, 2000.0f,
    80.0f, 0.0f, 1.0f, 1.0f, 0.0f, 25000.0f, 3.0f
  };

float dft_gts_inputs[GTS_SZ_IN] = /* 68 */
  {
    0.0f, 70.0f, 294.0f, 4.0f, 2.0f, 15.7f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 2.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1998.0f, 15.0f, 10.0f, 15.0f, 0.0f,
    1.0f, 1.0f, 0.5f, 1.0f, 1.0f, 2.0f, 0.5f, 1.0f, 1.0f, 1.0f,
    1.0f, 100.0f, 6.67f, 4.75f, 0.0f, 1.0f, 1.0f, 40.0f, 30.0f, 95.0f,
    98.0f, 250.0f, 9000.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f
  };

float dft_stk_inputs[1] = { 0.0f }; // stk has no inputs

float dft_sum_inputs[1] = { 0.0f }; // sum has no inputs

float dft_cseq_inputs[CSEQ_SZ_IN] = { 0.0f };

int i;

	for (i=0; i<CMN_SZ_IN; i++)
		cmn_inputs[i] = dft_cmn_inputs[i];

	for (i=0; i<IGCC_SZ_IN; i++)
		igcc_inputs[i] = dft_igcc_inputs[i];

	for (i=0; i<ASU_SZ_IN; i++)
		asu_inputs[i] = dft_asu_inputs[i];

	for (i=0; i<TEX_SZ_IN; i++)
		tex_inputs[i] = dft_tex_inputs[i];

	for (i=0; i<WGSR_SZ_IN; i++)
		wgsr_inputs[i] = dft_wgsr_inputs[i];

	for (i=0; i<SRS_SZ_IN; i++)
		srs_inputs[i] = dft_srs_inputs[i];

	for (i=0; i<SELX_SZ_IN; i++)
		selx_inputs[i] = dft_selx_inputs[i];

	for (i=0; i<GTS_SZ_IN; i++)
		gts_inputs[i] = dft_gts_inputs[i];

	for (i=0; i<CSEQ_SZ_IN; i++)
		cseq_inputs[i] = 0.0f;
}

int cmu_model::Initialization()
{
  printf ("[ Initializing... ]\n");
  error = 0;
  init_link_tp();
  if (error) 
	  return -1;
  init_vars();
  if (error) 
	  return -1;
  init_runfns();
  if (error) 
	  return -1;
  printf ("[ Done. ]\n");
  return 0;
}

/* initialize the link_tp array - this is ugly, but it's the easiest way to */
/* combine emacs with a copy and paste from the db (this will eventually be */
/* generated by the interface compiler) */

void cmu_model::init_link_tp ()
{
  int i;

  for (i=0; i<LINK_SZ; i++) link_tp[i] = 0;
  link_tp[0] = 4;
  link_tp[1] = 3;
  link_tp[13] = 3;
  link_tp[25] = 3;
  link_tp[37] = 3;
  link_tp[46] = 3;
  link_tp[49] = 3;
  link_tp[50] = 3;
  link_tp[51] = 3;
  link_tp[63] = 3;
  link_tp[75] = 3;
  link_tp[87] = 3;
  link_tp[99] = 3;
  link_tp[100] = 3;
  link_tp[101] = 3;
  link_tp[110] = 3;
  link_tp[119] = 3;
  link_tp[128] = 3;
  link_tp[137] = 3;
  link_tp[146] = 3;
  link_tp[155] = 3;
  link_tp[158] = 3;
  link_tp[161] = 3;
  link_tp[164] = 4;
  link_tp[165] = 4;
  link_tp[166] = 4;
  link_tp[167] = 4;
  link_tp[168] = 4;
  link_tp[169] = 4;
  link_tp[170] = 3;
  link_tp[171] = 3;
  link_tp[172] = 3;
  link_tp[173] = 3;
  link_tp[174] = 4;
  link_tp[175] = 4;
  link_tp[176] = 4;
  link_tp[177] = 4;
  link_tp[178] = 4;
  link_tp[179] = 4;
  link_tp[180] = 4;
  link_tp[181] = 4;
  link_tp[182] = 4;
  link_tp[183] = 4;
  link_tp[184] = 4;
  link_tp[185] = 4;
  link_tp[186] = 3;
  link_tp[187] = 3;
  link_tp[188] = 3;
  link_tp[192] = 3;
  link_tp[196] = 3;
  link_tp[200] = 3;
  link_tp[204] = 3;
  link_tp[208] = 3;
  link_tp[212] = 3;
  link_tp[216] = 3;
  link_tp[220] = 3;
  link_tp[224] = 3;
  link_tp[228] = 3;
  link_tp[229] = 4;
  link_tp[269] = 4;
  link_tp[270] = 4;
  link_tp[282] = 4;
  link_tp[295] = 4;
  link_tp[296] = 4;
  link_tp[297] = 4;
  link_tp[298] = 4;
  link_tp[299] = 4;
  link_tp[300] = 4;
  link_tp[301] = 4;
  link_tp[302] = 4;
  link_tp[303] = 4;
  link_tp[304] = 4;
  link_tp[305] = 3;
  link_tp[306] = 4;
  link_tp[307] = 4;
  link_tp[308] = 4;
  link_tp[309] = 4;
  link_tp[310] = 4;
  link_tp[311] = 4;
  link_tp[312] = 4;
  link_tp[315] = 4;
  link_tp[318] = 4;
  link_tp[319] = 4;
  link_tp[320] = 4;
  link_tp[321] = 4;
  link_tp[322] = 4;
  link_tp[323] = 4;
  link_tp[324] = 4;
  link_tp[325] = 4;
} /* void init_link_tp() */

/* set up the variable arrays */
void cmu_model::init_vars ()
{
  int i;

  for (i=0; i<LINK_SZ; i++) link_gen[i] = 0.0f;

  for (i=0; i<=PREFIX_IDX_MAX; i++){
    input_vars[i] = NULL;
    input_sz[i] = 0;
    calc_vars[i] = NULL;
    calc_sz[i] = 0;
    output_vars[i] = NULL;
    output_sz[i] = 0;
  } /* for i */

  input_vars[INIT_IDX] = init_inputs;
  calc_vars[INIT_IDX] = init_calcs;
  output_vars[INIT_IDX] = init_outputs;
  input_sz[INIT_IDX] = INIT_SZ_IN;
  calc_sz[INIT_IDX] = INIT_SZ_CALC;
  output_sz[INIT_IDX] = INIT_SZ_OUT;

  input_vars[CMN_IDX] = cmn_inputs;
  calc_vars[CMN_IDX] = cmn_calcs;
  output_vars[CMN_IDX] = cmn_outputs;
  input_sz[CMN_IDX] = CMN_SZ_IN;
  calc_sz[CMN_IDX] = CMN_SZ_CALC;
  output_sz[CMN_IDX] = CMN_SZ_OUT;

  input_vars[IGCC_IDX] = igcc_inputs;
  calc_vars[IGCC_IDX] = igcc_calcs;
  output_vars[IGCC_IDX] = igcc_outputs;
  input_sz[IGCC_IDX] = IGCC_SZ_IN;
  calc_sz[IGCC_IDX] = IGCC_SZ_CALC;
  output_sz[IGCC_IDX] = IGCC_SZ_OUT;

  input_vars[COAL_IDX] = coal_inputs;
  calc_vars[COAL_IDX] = coal_calcs;
  output_vars[COAL_IDX] = coal_outputs;
  input_sz[COAL_IDX] = COAL_SZ_IN;
  calc_sz[COAL_IDX] = COAL_SZ_CALC;
  output_sz[COAL_IDX] = COAL_SZ_OUT;

  input_vars[ASU_IDX] = asu_inputs;
  calc_vars[ASU_IDX] = asu_calcs;
  output_vars[ASU_IDX] = asu_outputs;
  input_sz[ASU_IDX] = ASU_SZ_IN;
  calc_sz[ASU_IDX] = ASU_SZ_CALC;
  output_sz[ASU_IDX] = ASU_SZ_OUT;

  input_vars[TEX_IDX] = tex_inputs;
  calc_vars[TEX_IDX] = tex_calcs;
  output_vars[TEX_IDX] = tex_outputs;
  input_sz[TEX_IDX] = TEX_SZ_IN;
  calc_sz[TEX_IDX] = TEX_SZ_CALC;
  output_sz[TEX_IDX] = TEX_SZ_OUT;

  input_vars[WGSR_IDX] = wgsr_inputs;
  calc_vars[WGSR_IDX] = wgsr_calcs;
  output_vars[WGSR_IDX] = wgsr_outputs;
  input_sz[WGSR_IDX] = WGSR_SZ_IN;
  calc_sz[WGSR_IDX] = WGSR_SZ_CALC;
  output_sz[WGSR_IDX] = WGSR_SZ_OUT;

  input_vars[SRS_IDX] = srs_inputs;
  calc_vars[SRS_IDX] = srs_calcs;
  output_vars[SRS_IDX] = srs_outputs;
  input_sz[SRS_IDX] = SRS_SZ_IN;
  calc_sz[SRS_IDX] = SRS_SZ_CALC;
  output_sz[SRS_IDX] = SRS_SZ_OUT;

  input_vars[SELX_IDX] = selx_inputs;
  calc_vars[SELX_IDX] = selx_calcs;
  output_vars[SELX_IDX] = selx_outputs;
  input_sz[SELX_IDX] = SELX_SZ_IN;
  calc_sz[SELX_IDX] = SELX_SZ_CALC;
  output_sz[SELX_IDX] = SELX_SZ_OUT;

  input_vars[GTS_IDX] = gts_inputs;
  calc_vars[GTS_IDX] = gts_calcs;
  output_vars[GTS_IDX] = gts_outputs;
  input_sz[GTS_IDX] = GTS_SZ_IN;
  calc_sz[GTS_IDX] = GTS_SZ_CALC;
  output_sz[GTS_IDX] = GTS_SZ_OUT;

  input_vars[STK_IDX] = stk_inputs;
  calc_vars[STK_IDX] = stk_calcs;
  output_vars[STK_IDX] = stk_outputs;
  input_sz[STK_IDX] = STK_SZ_IN;
  calc_sz[STK_IDX] = STK_SZ_CALC;
  output_sz[STK_IDX] = STK_SZ_OUT;

  input_vars[CSEQ_IDX] = cseq_inputs;
  calc_vars[CSEQ_IDX] = cseq_calcs;
  output_vars[CSEQ_IDX] = cseq_outputs;
  input_sz[CSEQ_IDX] = CSEQ_SZ_IN;
  calc_sz[CSEQ_IDX] = CSEQ_SZ_CALC;
  output_sz[CSEQ_IDX] = CSEQ_SZ_OUT;

  input_vars[SUM_IDX] = sum_inputs;
  calc_vars[SUM_IDX] = sum_calcs;
  output_vars[SUM_IDX] = sum_outputs;
  input_sz[SUM_IDX] = SUM_SZ_IN;
  calc_sz[SUM_IDX] = SUM_SZ_CALC;
  output_sz[SUM_IDX] = SUM_SZ_OUT;

  for (i=0,general_io_sz=LINK_SZ; i<=PREFIX_IDX_MAX; i++){
    if (general_io_sz < input_sz[i]) general_io_sz = input_sz[i];
    if (general_io_sz < calc_sz[i]) general_io_sz = calc_sz[i];
    if (general_io_sz < output_sz[i]) general_io_sz = output_sz[i];
  } /* for i */
  general_io = (float*) malloc (general_io_sz * sizeof(float));
  if (general_io == NULL){
    printf ("[ unable to allocate memory for i/o array ]\n");
    error = -1;
  } /* if !general_io */
  for (i=0; i<general_io_sz; i++) general_io[i] = 0.0f;
} /* void init_vars() */

/* look up the module functions in the dll */
void cmu_model::lookup_fns (int idx, prefixfn_name_array_t fnlist)
{
  /* Assume everything is in model.dll - this isn't technically required */
  /* to be true, but we have no plans to change it. */

  if (*fnlist[PREFIXFN_INIT] != '\0'){
    runfns[idx].init =
      (prefixfn_init_fn_t) GetProcAddress (model_dll, fnlist[PREFIXFN_INIT]);
    if (runfns[idx].init == NULL){
      printf ("[ unable to locate function '%s'. ]\n", fnlist[PREFIXFN_INIT]);
      error = -1;
      return;
    } /* if !runfns[idx].init */
  } /* if PREFIXFN_INIT */

  if (*fnlist[PREFIXFN_SETINPUTS] != '\0'){
    runfns[idx].setinputs =
      (prefixfn_interface_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_SETINPUTS]);
    if (runfns[idx].setinputs == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_SETINPUTS]);
      error = -1;
      return;
    } /* if !runfns[idx].setinputs */
  } /* if PREFIXFN_SETINPUTS */

  if (*fnlist[PREFIXFN_SETLINKS] != '\0'){
    runfns[idx].setlinks =
      (prefixfn_interface_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_SETLINKS]);
    if (runfns[idx].setlinks == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_SETLINKS]);
      error = -1;
      return;
    } /* if !runfns[idx].setlinks */
  } /* if PREFIXFN_SETLINKS */

  if (*fnlist[PREFIXFN_GETLINKS] != '\0'){
    runfns[idx].getlinks =
      (prefixfn_interface_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_GETLINKS]);
    if (runfns[idx].getlinks == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_GETLINKS]);
      error = -1;
      return;
    } /* if !runfns[idx].getlinks */
  } /* if PREFIXFN_GETLINKS */

  if (*fnlist[PREFIXFN_GETCALC] != '\0'){
    runfns[idx].getcalc =
      (prefixfn_interface_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_GETCALC]);
    if (runfns[idx].getcalc == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_GETCALC]);
      error = -1;
      return;
    } /* if !runfns[idx].getcalc */
  } /* if PREFIXFN_GETCALC */

  if (*fnlist[PREFIXFN_GETRESULTS] != '\0'){
    runfns[idx].getresults =
      (prefixfn_interface_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_GETRESULTS]);
    if (runfns[idx].getresults == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_GETRESULTS]);
      error = -1;
      return;
    } /* if !runfns[idx].getresults */
  } /* if PREFIXFN_GETRESULTS */

  if (*fnlist[PREFIXFN_SETCONFIG] != '\0'){
    runfns[idx].setconfig =
      (prefixfn_setconfig_fn_t) GetProcAddress (model_dll,
						fnlist[PREFIXFN_SETCONFIG]);
    if (runfns[idx].setconfig == NULL){
      printf ("[ unable to locate function '%s'. ]\n",
	      fnlist[PREFIXFN_SETCONFIG]);
      error = -1;
      return;
    } /* if !runfns[idx].setconfig */
  } /* if PREFIXFN_SETCONFIG */
} /* void lookup_fns() */

/* set up the function arrays */
void cmu_model::init_runfns ()
{
  int i;

  model_dll = LoadLibrary ("model.dll");
  if (model_dll == NULL){
    printf ("[ unable to load model.dll ]\n");
    error = -1;
    return;
  } /* if !model_dll */

  for (i=0; i<=PREFIX_IDX_MAX; i++){
    runfns[i].init = NULL;
    runfns[i].setinputs = NULL;
    runfns[i].setlinks = NULL;
    runfns[i].getlinks = NULL;
    runfns[i].getcalc = NULL;
    runfns[i].getresults = NULL;
    runfns[i].setconfig = NULL;
  } /* for i */

  lookup_fns (INIT_IDX, INIT_PREFIXFNS);
  lookup_fns (CMN_IDX, CMN_PREFIXFNS);
  lookup_fns (IGCC_IDX, IGCC_PREFIXFNS);
  lookup_fns (COAL_IDX, COAL_PREFIXFNS);
  lookup_fns (ASU_IDX, ASU_PREFIXFNS);
  lookup_fns (TEX_IDX, TEX_PREFIXFNS);
  lookup_fns (WGSR_IDX, WGSR_PREFIXFNS);
  lookup_fns (SRS_IDX, SRS_PREFIXFNS);
  lookup_fns (SELX_IDX, SELX_PREFIXFNS);
  lookup_fns (GTS_IDX, GTS_PREFIXFNS);
  lookup_fns (STK_IDX, STK_PREFIXFNS);
  lookup_fns (CSEQ_IDX, CSEQ_PREFIXFNS);
  lookup_fns (SUM_IDX, SUM_PREFIXFNS);
 
  for (i=0; i<=PREFIX_IDX_MAX; i++){
    if (runfns[i].init != NULL) runfns[i].init();
  } /* for i */
} /* void init_runfns() */

/* ----- end setup functions ----- */

//////////////////////// MISC and Link functions //////////////////////////
/* floating point exception translation */
void exception_trans_func (unsigned int e, _EXCEPTION_POINTERS *pExp)
{
  switch (e){
    case EXCEPTION_FLT_INVALID_OPERATION:
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      throw (unsigned int)EXCEPTION_FLT_DIVIDE_BY_ZERO;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      throw e;
  } /* switch e */
} /* void exception_trans_func() */

/* clear links */
void cmu_model::clear_links (float *links, int sz)
{
  int i;
  for (i=0; i<sz; i++) links[i] = 0.0f;
} /* void clear_links() */

/* copy links (copy_inputs, copy_outputs, copy_links all reduce to the same */
/* code w/o the database - one of the few things that gets simpler) */
void cmu_model::copy_links (float *to, float *from, int sz)
{
  int i;
  for (i=0; i<sz; i++) to[i] = from[i];
} /* void copy_links() */

/* copy links, comparing on the way by */
/* if linktp is specified, don't copy loop variables */
int cmu_model::cmp_links (float *to, float *from, int sz, int *linktp)
{
  int i, retval, lasttp;
  float close_enough;

  for (i=0, retval=0, lasttp=3; i<sz; i++){
    if (linktp != NULL){
      if (linktp[i] != 0) lasttp = linktp[i];
      if (lasttp == 3) continue;
    } /* if linktp */
    close_enough = (float)(fabs((double)to[i])) * 0.0005f;
    if ((from[i] < (to[i] - close_enough)) ||
	(from[i] > (to[i] + close_enough))) retval = 1;
    to[i] = from[i];
  } /* for i */

  return (retval);
} /* int cmp_links() */

/* ----- begin run functions ----- */
/* set module inputs */
void cmu_model::set_modinputs (int *runconf, int runconf_sz, int runconf_id)
{
  int i;
  int cid[1];
  int errcode[1];

  *cid = runconf_id;
  *errcode = 0;
  for (i=0; i<runconf_sz; i++){
    if (runconf[i] <= 0) continue;
    if (runfns[runconf[i]].setconfig != NULL){
      runfns[runconf[i]].setconfig (cid, errcode);
      if (*errcode != 0){
	printf ("[ error in setconfig ]\n");
	error = -1;
	break;
      } /* if errcode != 0 */
    } /* for i */
    if (error) return;

    if (runfns[runconf[i]].setinputs == NULL){
      printf ("[ unable to call setinputs ]\n");
      error = -1;
      break;
    } /* if !setinputs */
    copy_links (general_io, input_vars[runconf[i]], input_sz[runconf[i]]);
    runfns[runconf[i]].setinputs (general_io);
  } /* for i */
} /* void set_modinputs() */

/* get module outputs */
void cmu_model::get_modoutputs (int *runconf, int runconf_sz)
{
  int i;

  for (i=0; i<runconf_sz; i++){
    if (runconf[i] <= 0) continue;
    if (output_sz[runconf[i]] > 0){
      if (runfns[runconf[i]].getresults == NULL){
	printf ("[ unable to call getresults ]\n");
	error = -1;
	break;
      } /* if !getresults */

      clear_links (general_io, output_sz[runconf[i]]);
      runfns[runconf[i]].getresults (general_io);
      copy_links(output_vars[runconf[i]], general_io, output_sz[runconf[i]]);
    } /* if output_sz > 0 */

    if (calc_sz[runconf[i]] > 0){
      if (runfns[runconf[i]].getcalc == NULL){
	printf ("[ unable to call getcalc ]\n");
	error = -1;
	break;
      } /* if !getcalc */

      clear_links (general_io, calc_sz[runconf[i]]);
      runfns[runconf[i]].getcalc (general_io);
      copy_links (calc_vars[runconf[i]], general_io, calc_sz[runconf[i]]);
    } /* if calc_sz > 0 */
  } /* for i */
} /* void get_modoutputs() */

/* run the specified modules */
int cmu_model::run_modules (int *runconf, int runconf_sz)
{
  float link_cmp[LINK_SZ];
  int i, j, k, iter, *ml, sz, lastidx;

  if (runconf[0] < 0){
    iter = -runconf[0];
    if (runconf_sz < 2){
      printf ("[ unterminated loop in run config ]\n");
      error = -1;
      return (-1);
    } /* if runconf_sz < 2 */
    ml = &runconf[1];
    sz = runconf_sz - 1;
    for (i=0; i<LINK_SZ; i++) link_cmp[i] = 0.0f;
  }else{
    iter = -1;
    ml = runconf;
    sz = runconf_sz;
  } /* if runconf[0] < 0 */

  i = 0;
  lastidx = 0;
  do{
    if (i > 0){
      if (!cmp_links (link_cmp, general_io, LINK_SZ, link_tp)) break;
    }else{
      if (iter < 0){
	copy_links (link_gen, general_io, LINK_SZ);
      }else{
	copy_links (link_cmp, general_io, LINK_SZ);
      } /* if iter < 0 */
    } /* if i>0 */
    for (j=0; j<sz; j++){
      if (j > lastidx) lastidx = j;
      if (ml[j] == 0) break;
      if (ml[j] < 0){
	k = run_modules (&ml[j], sz-j);
	if (k < 0) return (-1);
	j += k;
	if (j >= sz) return (-1);
	if (j > lastidx) lastidx = j;
      }else{
	if (runfns[ml[j]].setlinks == NULL){
	  printf ("[ unable to run setlinks ]\n");
	  error = -1;
	  return (-1);
	} /* if !setlinks */
	if (j == 0){
	  if (iter < 0){
	    runfns[ml[j]].setlinks (link_gen);
	    copy_links (general_io, link_gen, LINK_SZ);
	  }else{
	    runfns[ml[j]].setlinks (link_cmp);
	    copy_links (general_io, link_cmp, LINK_SZ);
	  } /* if iter < 0 */
	}else{
	  copy_links (link_gen, general_io, LINK_SZ);
	  runfns[ml[j]].setlinks (link_gen);
	  copy_links (general_io, link_gen, LINK_SZ);
	} /* if j == 0 */
	if (runfns[ml[j]].getlinks == NULL){
	  printf ("[ unable to run getlinks ]\n");
	  error = -1;
	  return (-1);
	} /* if !getlinks */
	runfns[ml[j]].getlinks (general_io);
	
	//Log the link contents here
	LogLink(ml[j], general_io, linkLog);

      } /* if ml[j] < 0 */
    } /* for j */
    i++;
  }while (i < iter);

  if ((i >= iter) && (iter > 0)){
    printf ("[ warning: maximum iteration count exceeded ]\n");
  } /* if max iter */

  if (iter > 0) lastidx++;
  return (lastidx);
} /* void run_modules */

/* run the model with the specified configuration */
void cmu_model::run_orig_model (int *runconf, int runconf_sz, int runconf_id)
{
  int i, j;
  _clearfp();
  {
    void (*tr)( unsigned int, struct _EXCEPTION_POINTERS*)  =
      _set_se_translator (exception_trans_func);
    int oldcw = _controlfp (0, 0);
    int cw;
    cw = oldcw & ~(EM_ZERODIVIDE|EM_INVALID);
    _controlfp (cw, MCW_EM);
    try{
      /* set the inputs */
      set_modinputs (runconf, runconf_sz, runconf_id);

      /* run the model */
      if (!error){
		clear_links (general_io, general_io_sz);
		i=0;
		do{
			j = run_modules (&runconf[i], runconf_sz-i);
			i += j + 1;
			if (j < 0){
				printf("[ an error occured while attempting to run the model ]\n");
				error = -1;
				break;
			} /* if j < 0 */
		}while (i < runconf_sz-1);
      } /* if !error */

      /* get the outputs */
      if (!error) get_modoutputs (runconf, runconf_sz);
	}catch (unsigned int){
      _clearfp();
      printf ("[ an error occured while attempting to run the model ]\n");
      error = -1;
    } /* try */
    _clearfp();
    _controlfp (oldcw, MCW_EM);
    _set_se_translator (tr);
  }
} /* void run_model() */


void cmu_model::run_model_withlink(int *runconf, int runconf_sz, int runconf_id, float* link, int sz)
{
  int i, j;
  _clearfp();
  {
    void (*tr)( unsigned int, struct _EXCEPTION_POINTERS*)  =
      _set_se_translator (exception_trans_func);
    int oldcw = _controlfp (0, 0);
    int cw;
    cw = oldcw & ~(EM_ZERODIVIDE|EM_INVALID);
    _controlfp (cw, MCW_EM);
    try{
      /* set the inputs */
      set_modinputs (runconf, runconf_sz, runconf_id);

      /* run the model */
      if (!error){
		//clear the links
		clear_links (general_io, general_io_sz);
		//reset the link with the passed value
		copy_links (general_io, link, sz);
		i=0;
		do{
			j = run_modules (&runconf[i], runconf_sz-i);
			i += j + 1;
			if (j < 0){
				printf("[ an error occured while attempting to run the model ]\n");
				error = -1;
				break;
			} /* if j < 0 */
		}while (i < runconf_sz-1);
      } /* if !error */

      /* get the outputs */
      if (!error) get_modoutputs (runconf, runconf_sz);
	}catch (unsigned int){
      _clearfp();
      printf ("[ an error occured while attempting to run the model ]\n");
      error = -1;
    } /* try */
    _clearfp();
    _controlfp (oldcw, MCW_EM);
    _set_se_translator (tr);
  }

}
/* ----- end run functions ----- */

//Logging link contents function
void cmu_model::LogLink(int module_id, float*curlink, float savelink[PREFIX_IDX_MAX][LINK_SZ])
{
	unsigned int i; 

	if(module_id==28 && _rei_gasi) {
       
       double tot = 0;
	   for(i=0; i<LINK_SZ_SYNGAS; i++)
				tot += curlink[LINK_IDX_SYNGAS+i];

       curlink[LINK_IDX_SYNGAS+0] = 0.425533;    // CO
       curlink[LINK_IDX_SYNGAS+1] = 0.295978;   // H2
       curlink[LINK_IDX_SYNGAS+2] = 0.000386;        // CH4
       curlink[LINK_IDX_SYNGAS+3] = 0.005314;  // H2S
       curlink[LINK_IDX_SYNGAS+4] = 0.000257;    // COS
       curlink[LINK_IDX_SYNGAS+5] = 0.0;        // NH3
       curlink[LINK_IDX_SYNGAS+6] = 0.000142;        // HCl
       curlink[LINK_IDX_SYNGAS+7] = 0.08908;  // CO2
       curlink[LINK_IDX_SYNGAS+8] = 0.16651;   // H2O
       curlink[LINK_IDX_SYNGAS+9] = 0.00762;  // N2
       curlink[LINK_IDX_SYNGAS+10] = 0.00915; // Ar
       curlink[LINK_IDX_SYNGAS+11] = 0.0;       // O2

       double tot2 = 0;
	   for(i=0; i<LINK_SZ_SYNGAS; i++)
				tot2 += curlink[LINK_IDX_SYNGAS+i];
 
   	   for(i=0; i<LINK_SZ_SYNGAS; i++)
		   curlink[LINK_IDX_SYNGAS+i] = curlink[LINK_IDX_SYNGAS+i]/tot2*tot;
 	}


	for (i=0; i<LINK_SZ; i++)
		savelink[module_id][i]=curlink[i];

}
