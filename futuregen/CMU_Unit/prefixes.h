/* prefixes.h -- index & other prefix-related constants */
/* Generated from 'K:\ky\IECM_4.0.0\c\veggy\interface\interface_igcc.cfg' on 11/14/03 13:11:32. */
/* Version IECM Interface 4.0 */

#define PREFIXFN_DLL 0
#define PREFIXFN_INIT 1
#define PREFIXFN_SETINPUTS 2
#define PREFIXFN_SETLINKS 3
#define PREFIXFN_GETLINKS 4
#define PREFIXFN_GETCALC 5
#define PREFIXFN_GETRESULTS 6
#define PREFIXFN_SETCONFIG 7
typedef char *prefixfn_name_array_t[8];

typedef void (__stdcall *prefixfn_init_fn_t) ();
typedef void (__stdcall *prefixfn_interface_fn_t) (float a[]);
typedef void (__stdcall *prefixfn_setconfig_fn_t) (int config[], int error[]);
typedef struct {
	prefixfn_init_fn_t init;
	prefixfn_interface_fn_t setinputs;
	prefixfn_interface_fn_t setlinks;
	prefixfn_interface_fn_t getlinks;
	prefixfn_interface_fn_t getcalc;
	prefixfn_interface_fn_t getresults;
	prefixfn_setconfig_fn_t setconfig;
} prefixfn_fn_array_t;

#define LINK_IDX 0
#define BASH_IDX 1
#define FASH_IDX 2
#define FGT_IDX 3
#define PLT_IDX 4
#define APH_IDX 5
#define HSCR_IDX 6
#define CESP_IDX 7
#define FF_IDX 8
#define WET_IDX 9
#define DRY_IDX 10
#define NXSO_IDX 11
#define CUO_IDX 12
#define SUL_IDX 13
#define ACID_IDX 14
#define CMN_IDX 15
#define COAL_IDX 16
#define OIL_IDX 17
#define NGAS_IDX 18
#define SUM_IDX 19
#define SNCR_IDX 20
#define FGC_IDX 21
#define ABCC_IDX 22
#define CSEQ_IDX 23
#define IGCC_IDX 24
#define GTS_IDX 25
#define STK_IDX 26
#define DST_IDX 27
#define TEX_IDX 28
#define HTC_IDX 29
#define ASU_IDX 30
#define SRS_IDX 31
#define AIR_IDX 32
#define SELX_IDX 33
#define WGSR_IDX 34
#define _ED__IDX 35
#define INIT_IDX 36
#define BOIL_IDX 37
#define STCK_IDX 38
#define UNITS_IDX 39
#define PREFIX_IDX_MAX 39
