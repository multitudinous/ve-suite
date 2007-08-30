

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 6.00.0366 */
/* at Thu Aug 30 17:04:03 2007
 */
/* Compiler settings for happ.idl:
    Oicf, W1, Zp8, env=Win32 (32b run)
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
//@@MIDL_FILE_HEADING(  )

#pragma warning( disable: 4049 )  /* more than 64k source lines */


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__


#ifndef __happ_h__
#define __happ_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __IHNode_FWD_DEFINED__
#define __IHNode_FWD_DEFINED__
typedef interface IHNode IHNode;
#endif 	/* __IHNode_FWD_DEFINED__ */


#ifndef __IHapp_FWD_DEFINED__
#define __IHapp_FWD_DEFINED__
typedef interface IHapp IHapp;
#endif 	/* __IHapp_FWD_DEFINED__ */


#ifndef __IHSelection_FWD_DEFINED__
#define __IHSelection_FWD_DEFINED__
typedef interface IHSelection IHSelection;
#endif 	/* __IHSelection_FWD_DEFINED__ */


#ifndef __IHAPEngine_FWD_DEFINED__
#define __IHAPEngine_FWD_DEFINED__
typedef interface IHAPEngine IHAPEngine;
#endif 	/* __IHAPEngine_FWD_DEFINED__ */


#ifndef __IParentAdviseSink_FWD_DEFINED__
#define __IParentAdviseSink_FWD_DEFINED__
typedef interface IParentAdviseSink IParentAdviseSink;
#endif 	/* __IParentAdviseSink_FWD_DEFINED__ */


#ifndef __IHAPLibRef_FWD_DEFINED__
#define __IHAPLibRef_FWD_DEFINED__
typedef interface IHAPLibRef IHAPLibRef;
#endif 	/* __IHAPLibRef_FWD_DEFINED__ */


#ifndef __IEnumAModelLibrary_FWD_DEFINED__
#define __IEnumAModelLibrary_FWD_DEFINED__
typedef interface IEnumAModelLibrary IEnumAModelLibrary;
#endif 	/* __IEnumAModelLibrary_FWD_DEFINED__ */


#ifndef __IHNodeCol_FWD_DEFINED__
#define __IHNodeCol_FWD_DEFINED__
typedef interface IHNodeCol IHNodeCol;
#endif 	/* __IHNodeCol_FWD_DEFINED__ */


#ifndef __IScrollAreaInfo_FWD_DEFINED__
#define __IScrollAreaInfo_FWD_DEFINED__
typedef interface IScrollAreaInfo IScrollAreaInfo;
#endif 	/* __IScrollAreaInfo_FWD_DEFINED__ */


#ifndef __IHPlotVal_FWD_DEFINED__
#define __IHPlotVal_FWD_DEFINED__
typedef interface IHPlotVal IHPlotVal;
#endif 	/* __IHPlotVal_FWD_DEFINED__ */


#ifndef __IAPPDF_FWD_DEFINED__
#define __IAPPDF_FWD_DEFINED__
typedef interface IAPPDF IAPPDF;
#endif 	/* __IAPPDF_FWD_DEFINED__ */


#ifndef __IHAPHandle_FWD_DEFINED__
#define __IHAPHandle_FWD_DEFINED__
typedef interface IHAPHandle IHAPHandle;
#endif 	/* __IHAPHandle_FWD_DEFINED__ */


#ifndef __IAPropData_FWD_DEFINED__
#define __IAPropData_FWD_DEFINED__
typedef interface IAPropData IAPropData;
#endif 	/* __IAPropData_FWD_DEFINED__ */


#ifndef __IAPPasteItems_FWD_DEFINED__
#define __IAPPasteItems_FWD_DEFINED__
typedef interface IAPPasteItems IAPPasteItems;
#endif 	/* __IAPPasteItems_FWD_DEFINED__ */


#ifndef __IAPPasteItem_FWD_DEFINED__
#define __IAPPasteItem_FWD_DEFINED__
typedef interface IAPPasteItem IAPPasteItem;
#endif 	/* __IAPPasteItem_FWD_DEFINED__ */


#ifndef __IHSelectionCallback_FWD_DEFINED__
#define __IHSelectionCallback_FWD_DEFINED__
typedef interface IHSelectionCallback IHSelectionCallback;
#endif 	/* __IHSelectionCallback_FWD_DEFINED__ */


#ifndef __IAPHappEvents_FWD_DEFINED__
#define __IAPHappEvents_FWD_DEFINED__
typedef interface IAPHappEvents IAPHappEvents;
#endif 	/* __IAPHappEvents_FWD_DEFINED__ */


#ifndef __IHComposite_FWD_DEFINED__
#define __IHComposite_FWD_DEFINED__
typedef interface IHComposite IHComposite;
#endif 	/* __IHComposite_FWD_DEFINED__ */


#ifndef __IHWizardPlot_FWD_DEFINED__
#define __IHWizardPlot_FWD_DEFINED__
typedef interface IHWizardPlot IHWizardPlot;
#endif 	/* __IHWizardPlot_FWD_DEFINED__ */


#ifndef __IHAdhocPlot_FWD_DEFINED__
#define __IHAdhocPlot_FWD_DEFINED__
typedef interface IHAdhocPlot IHAdhocPlot;
#endif 	/* __IHAdhocPlot_FWD_DEFINED__ */


#ifndef __IMMControlVerb_FWD_DEFINED__
#define __IMMControlVerb_FWD_DEFINED__
typedef interface IMMControlVerb IMMControlVerb;
#endif 	/* __IMMControlVerb_FWD_DEFINED__ */


#ifndef __IHappServiceProvider_FWD_DEFINED__
#define __IHappServiceProvider_FWD_DEFINED__
typedef interface IHappServiceProvider IHappServiceProvider;
#endif 	/* __IHappServiceProvider_FWD_DEFINED__ */


#ifndef __IAPConflict_FWD_DEFINED__
#define __IAPConflict_FWD_DEFINED__
typedef interface IAPConflict IAPConflict;
#endif 	/* __IAPConflict_FWD_DEFINED__ */


#ifndef __IAPConflicts_FWD_DEFINED__
#define __IAPConflicts_FWD_DEFINED__
typedef interface IAPConflicts IAPConflicts;
#endif 	/* __IAPConflicts_FWD_DEFINED__ */


#ifndef __IAPGroup_FWD_DEFINED__
#define __IAPGroup_FWD_DEFINED__
typedef interface IAPGroup IAPGroup;
#endif 	/* __IAPGroup_FWD_DEFINED__ */


#ifndef __IAPPasteParam_FWD_DEFINED__
#define __IAPPasteParam_FWD_DEFINED__
typedef interface IAPPasteParam IAPPasteParam;
#endif 	/* __IAPPasteParam_FWD_DEFINED__ */


#ifndef __IAPPasteParams_FWD_DEFINED__
#define __IAPPasteParams_FWD_DEFINED__
typedef interface IAPPasteParams IAPPasteParams;
#endif 	/* __IAPPasteParams_FWD_DEFINED__ */


#ifndef __IAPPasteBuffer_FWD_DEFINED__
#define __IAPPasteBuffer_FWD_DEFINED__
typedef interface IAPPasteBuffer IAPPasteBuffer;
#endif 	/* __IAPPasteBuffer_FWD_DEFINED__ */


#ifndef __IHNodeSelection_FWD_DEFINED__
#define __IHNodeSelection_FWD_DEFINED__
typedef interface IHNodeSelection IHNodeSelection;
#endif 	/* __IHNodeSelection_FWD_DEFINED__ */


#ifndef __IHappConnectInfo_FWD_DEFINED__
#define __IHappConnectInfo_FWD_DEFINED__
typedef interface IHappConnectInfo IHappConnectInfo;
#endif 	/* __IHappConnectInfo_FWD_DEFINED__ */


#ifndef __HappLS_FWD_DEFINED__
#define __HappLS_FWD_DEFINED__

#ifdef __cplusplus
typedef class HappLS HappLS;
#else
typedef struct HappLS HappLS;
#endif /* __cplusplus */

#endif 	/* __HappLS_FWD_DEFINED__ */


#ifndef __HappIP_FWD_DEFINED__
#define __HappIP_FWD_DEFINED__

#ifdef __cplusplus
typedef class HappIP HappIP;
#else
typedef struct HappIP HappIP;
#endif /* __cplusplus */

#endif 	/* __HappIP_FWD_DEFINED__ */


#ifndef __CHReadOnlyCompositeNode_FWD_DEFINED__
#define __CHReadOnlyCompositeNode_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHReadOnlyCompositeNode CHReadOnlyCompositeNode;
#else
typedef struct CHReadOnlyCompositeNode CHReadOnlyCompositeNode;
#endif /* __cplusplus */

#endif 	/* __CHReadOnlyCompositeNode_FWD_DEFINED__ */


#ifndef __CHReadWriteCompositeNode_FWD_DEFINED__
#define __CHReadWriteCompositeNode_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHReadWriteCompositeNode CHReadWriteCompositeNode;
#else
typedef struct CHReadWriteCompositeNode CHReadWriteCompositeNode;
#endif /* __cplusplus */

#endif 	/* __CHReadWriteCompositeNode_FWD_DEFINED__ */


#ifndef __CHPlotVal_FWD_DEFINED__
#define __CHPlotVal_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHPlotVal CHPlotVal;
#else
typedef struct CHPlotVal CHPlotVal;
#endif /* __cplusplus */

#endif 	/* __CHPlotVal_FWD_DEFINED__ */


#ifndef __CHWizardPlot_FWD_DEFINED__
#define __CHWizardPlot_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHWizardPlot CHWizardPlot;
#else
typedef struct CHWizardPlot CHWizardPlot;
#endif /* __cplusplus */

#endif 	/* __CHWizardPlot_FWD_DEFINED__ */


#ifndef __CHAdhocPlot_FWD_DEFINED__
#define __CHAdhocPlot_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHAdhocPlot CHAdhocPlot;
#else
typedef struct CHAdhocPlot CHAdhocPlot;
#endif /* __cplusplus */

#endif 	/* __CHAdhocPlot_FWD_DEFINED__ */


#ifndef __HappAProp_FWD_DEFINED__
#define __HappAProp_FWD_DEFINED__

#ifdef __cplusplus
typedef class HappAProp HappAProp;
#else
typedef struct HappAProp HappAProp;
#endif /* __cplusplus */

#endif 	/* __HappAProp_FWD_DEFINED__ */


#ifndef __HappAPropIP_FWD_DEFINED__
#define __HappAPropIP_FWD_DEFINED__

#ifdef __cplusplus
typedef class HappAPropIP HappAPropIP;
#else
typedef struct HappAPropIP HappAPropIP;
#endif /* __cplusplus */

#endif 	/* __HappAPropIP_FWD_DEFINED__ */


#ifndef __CHAdhocPlotAProp_FWD_DEFINED__
#define __CHAdhocPlotAProp_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHAdhocPlotAProp CHAdhocPlotAProp;
#else
typedef struct CHAdhocPlotAProp CHAdhocPlotAProp;
#endif /* __cplusplus */

#endif 	/* __CHAdhocPlotAProp_FWD_DEFINED__ */


#ifndef __CHWizardPlotAProp_FWD_DEFINED__
#define __CHWizardPlotAProp_FWD_DEFINED__

#ifdef __cplusplus
typedef class CHWizardPlotAProp CHWizardPlotAProp;
#else
typedef struct CHWizardPlotAProp CHWizardPlotAProp;
#endif /* __cplusplus */

#endif 	/* __CHWizardPlotAProp_FWD_DEFINED__ */


#ifdef __cplusplus
extern "C"{
#endif 

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void * ); 


#ifndef __Happ_LIBRARY_DEFINED__
#define __Happ_LIBRARY_DEFINED__

/* library Happ */
/* [custom][custom][custom][helpstring][version][uuid] */ 



































typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0001
    {	IAP_MOVETO_BLOCK	= 0,
	IAP_MOVETO_CONVERGENCE	= 1,
	IAP_MOVETO_SENSITIVITY	= 2,
	IAP_MOVETO_CALCULATOR	= 3,
	IAP_MOVETO_TRANSFER	= 4,
	IAP_MOVETO_EQUIPMENT	= 5,
	IAP_MOVETO_ECONOMIC	= 6,
	IAP_MOVETO_UTILITY	= 7,
	IAP_MOVETO_PRESRELIEF	= 8,
	IAP_MOVETO_REGRESSION	= 9,
	IAP_MOVETO_BALANCE	= 10,
	IAP_MOVETO_START	= 11,
	IAP_MOVETO_MEASUREMENT	= 20
    } 	IAP_MOVETO_TYPE;

typedef /* [public][public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0002
    {	IAP_PP1A	= 0,
	IAP_PP1B	= 1,
	IAP_PP2A	= 2,
	IAP_PP2B	= 3,
	IAP_PP2C	= 4,
	IAP_INSERTLIB	= 5,
	IAP_COST	= 6,
	IAP_STREAMLIB	= 7,
	IAP_RUNDEF	= 8,
	IAP_LINKEROPT	= 9
    } 	IAP_ENGINEFILES;

typedef /* [public][public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0003
    {	IAP_RUN_EXPRESS	= 0,
	IAP_RUN_INTERTACTIVE	= 1,
	IAP_RUN_MUSTBECOMPLETE	= 2,
	IAP_RUN_COPY_DRS_PCES	= 3,
	IAP_RUN_COPY_DRS	= 4,
	IAP_RUN_COPY_PCES	= 5,
	IAP_RUN_COPY_RETRIEVED	= 6
    } 	IAP_RUN_OPTION;

typedef /* [public][public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0004
    {	IAP_STOPPOINT_BLOCK	= 1,
	IAP_STOPPOINT_CONVERGENCE	= 2,
	IAP_STOPPOINT_SENSITIVITY	= 3,
	IAP_STOPPOINT_CALCULATOR	= 4,
	IAP_STOPPOINT_TRANSFER	= 5,
	IAP_STOPPOINT_EQUIPMENT	= 6,
	IAP_STOPPOINT_ECONOMIC	= 7,
	IAP_STOPPOINT_UTILITY	= 8,
	IAP_STOPPOINT_PRESRELIEF	= 9,
	IAP_STOPPOINT_REGRESSION	= 10,
	IAP_STOPPOINT_BALANCE	= 11
    } 	IAP_STOPPOINT_TYPE;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0005
    {	IAP_REINIT_SIMULATION	= 4,
	IAP_REINIT_BLOCK	= 1,
	IAP_REINIT_CONVERGENCE	= 2,
	IAP_REINIT_STREAM	= 3
    } 	IAP_REINIT_TYPE;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0006
    {	HAP_REPORT_INTERACTIVE	= 0,
	HAP_REPORT_BLOCK	= 1,
	HAP_REPORT_CONVERGENCE	= 2,
	HAP_REPORT_COST	= 3,
	HAP_REPORT_ECONOMIC	= 4,
	HAP_REPORT_SENSITIVITY	= 5,
	HAP_REPORT_TRANSFER	= 6,
	HAP_REPORT_CALCULATOR	= 7,
	HAP_REPORT_UTILITY	= 8,
	HAP_REPORT_STREAMS	= 9,
	HAP_REPORT_BALANCE	= 10,
	HAP_REPORT_PRES_RELIEF	= 11,
	HAP_REPORT_REGRESSION	= 12,
	HAP_REPORT_REPORT	= 13,
	HAP_REPORT_TOC	= 14,
	HAP_REPORT_FLOWSHEET_BALANCE	= 15,
	HAP_REPORT_PROP_TABLE	= 16,
	HAP_REPORT_DESIGN_SPEC	= 17,
	HAP_REPORT_CONNECTING_STREAMS	= 18,
	HAP_REPORT_CONSTRAINT	= 19,
	HAP_REPORT_OPTIMIZATION	= 20
    } 	HAPReportType;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0007
    {	HAP_VALUE	= 0,
	HAP_RESERVED1	= 1,
	HAP_UNITROW	= 2,
	HAP_UNITCOL	= 3,
	HAP_RESERVED2	= 4,
	HAP_OPTIONLIST	= 5,
	HAP_RECORDTYPE	= 6,
	HAP_ENTERABLE	= 7,
	HAP_UPPERLIMIT	= 8,
	HAP_LOWERLIMIT	= 9,
	HAP_VALUEDEFAULT	= 10,
	HAP_USERENTERED	= 11,
	HAP_COMPSTATUS	= 12,
	HAP_BASIS	= 13,
	HAP_INOUT	= 14,
	HAP_PORTSEX	= 15,
	HAP_MULTIPORT	= 16,
	HAP_PORTTYPE	= 17,
	HAP_OUTVAR	= 18,
	HAP_PROMPT	= 19,
	HAP_PRETENDNOTENTERED	= 20,
	HAP_HELPFILENAME	= 21,
	HAP_HELPID	= 22,
	HAP_FIRSTPAIR	= 23,
	HAP_NODENAME	= 24,
	HAP_METHOD	= 25,
	HAP_MARKED	= 26,
	HAP_VOLATILE	= 27,
	HAP_SECTION	= 28,
	HAP_DEFNAME	= 29,
	HAP_CANADD	= 30,
	HAP_CANDELETE	= 31,
	HAP_CANRENAME	= 32,
	HAP_CANHIDE	= 33,
	HAP_CANREVEAL	= 34,
	HAP_CANCLEAR	= 35,
	HAP_CANCOPY	= 36,
	HAP_CANPASTE	= 37,
	HAP_HASCHILDREN	= 38,
	HAP_PLOTLABEL	= 39,
	HAP_BIRDCAGE	= 40,
	HAP_STREAMCLASS	= 42,
	HAP_HASCOMMENTS	= 43,
	HAP_CANHAVECOMMENTS	= 44,
	HAP_UNDERLYINGPATH	= 45,
	HAP_ISHIDDEN	= 47,
	HAP_HIDEVIEW	= 48,
	HAP_ANALYSISFLAG	= 49,
	HAP_SPECSTREAM	= 50,
	HAP_REORDER	= 51,
	HAP_ISREALSYMBOL	= 52,
	HAP_CANEXPORT	= 58,
	HAP_BASETYPE	= 59,
	HAP_HIERARCHYFLAG	= 63,
	HAP_HIERPATH	= 64,
	HAP_FULLNAME	= 65,
	HAP_CANTEMPLAPPEND	= 66,
	HAP_ACTIVATESTATE	= 67,
	HAP_CANIMPORT	= 68,
	HAP_HASEOMSG	= 69,
	HAP_SHOWEOMSG	= 70,
	HAP_EOEXPORT	= 72,
	HAP_EOIMPORT	= 73,
	HAP_NAVPATH	= 74,
	HAP_HIERNAME	= 76,
	HAP_REVEALLIST	= 77,
	HAP_DEFRECONCILE	= 78,
	HAP_EONODENAME	= 79,
	HAP_UOM	= 81,
	HAP_UOMSET	= 82
    } 	HAPAttributeNumber;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0008
    {	HAP_RESULTS_SUCCESS	= 1,
	HAP_NORESULTS	= 2,
	HAP_RESULTS_WARNINGS	= 4,
	HAP_RESULTS_INACCESS	= 8,
	HAP_RESULTS_INCOMPAT	= 16,
	HAP_RESULTS_ERRORS	= 32,
	HAP_INPUT_INCOMPLETE	= 64,
	HAP_INPUT_COMPLETE	= 128,
	HAP_INPUT_INACCESS	= 256,
	HAP_INPUT_NEUTRAL	= 512,
	HAP_UNRECONCILED	= 1024,
	HAP_RECONCILED	= 2048,
	HAP_DISABLED	= 4096,
	HAP_ENABLED	= 8192,
	HAP_EOSYNC	= 16384,
	HAP_EODISABLE	= 32768,
	HAP_EOFAIL	= 0x10000,
	HAP_EOERROR	= 0x20000
    } 	HAPCompStatusCode;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0009
    {	HAP_SPECIAL_ANALYSIS	= 0,
	HAP_SPECIAL_PURE	= 1,
	HAP_SPECIAL_UNIGROUP	= 2,
	HAP_SPECIAL_PSD	= 3,
	HAP_SPECIAL_SUBOBJECT	= 4,
	HAP_SPECIAL_STREAM	= 5
    } 	HAPSpecialNodeType;

typedef /* [public][public] */ 
enum __MIDL___MIDL_itf_happ_0000_0010
    {	HAPEXP_USRDEF	= 0,
	HAPEXP_BACKUP	= 1,
	HAPEXP_REPORT	= 2,
	HAPEXP_SUMMARY	= 3,
	HAPEXP_INPUT	= 4,
	HAPEXP_INPUT_GRAPHICS	= 5,
	HAPEXP_RUNMSG	= 6,
	HAPEXP_REPORT_INPUT	= 7,
	HAPEXP_REPORT_SUMMARY	= 8,
	HAPEXP_FLOWDYN	= 9,
	HAPEXP_PDYN	= 10,
	HAPEXP_DXF	= 11,
	HAPEXP_SUMXML	= 12,
	HAPEXP_HISTORY	= 13,
	HAPEXP_PFS	= 14,
	HAPEXP_PDF	= 15,
	HAPEXP_EOATSLV	= 16,
	HAPEXP_EOATACT	= 17
    } 	HAPEXPType;

typedef struct tagLARGE_INTEGER
    {
    INT64 QuadPart;
    } 	LARGE_INTEGER;

typedef struct tagULARGE_INTEGER
    {
    UINT64 QuadPart;
    } 	ULARGE_INTEGER;

typedef struct tagFILETIME
    {
    unsigned long dwLowDateTime;
    unsigned long dwHighDateTime;
    } 	FILETIME;

typedef /* [helpstring][uuid] */  DECLSPEC_UUID("24B82EB0-4A1F-11D1-8A70-0000C0237DF9") struct tagPLOTTEXT
    {
    double x;
    double y;
    BSTR text;
    long color;
    BSTR fontname;
    short alignment;
    short size;
    short style;
    short pad;
    } 	PLOTTEXT;

typedef /* [public] */ 
enum __MIDL___MIDL_itf_happ_0129_0001
    {	HAPP_RECONCILE_INPUT	= 1,
	HAPP_RECONCILE_ONLY	= 2,
	HAPP_RECONCILE_TP	= 4,
	HAPP_RECONCILE_TV	= 8,
	HAPP_RECONCILE_PV	= 16,
	HAPP_RECONCILE_CF	= 32,
	HAPP_RECONCILE_TF	= 64,
	HAPP_RECONCILE_MICFMOLE	= 128,
	HAPP_RECONCILE_MICFMASS	= 256,
	HAPP_RECONCILE_MICFSTDVOL	= 512,
	HAPP_RECONCILE_MITFMOLE	= 1024,
	HAPP_RECONCILE_MITFMASS	= 2048,
	HAPP_RECONCILE_MITFSTDVOL	= 4096,
	HAPP_RECONCILE_CICFMOLE	= 16384,
	HAPP_RECONCILE_CICFMASS	= 32768,
	HAPP_RECONCILE_CITFMOLE	= 0x10000,
	HAPP_RECONCILE_CITFMASS	= 0x20000,
	HAPP_RECONCILE_QUIET	= 0x100000,
	HAPP_RECONCILE_DONTASKWARN	= 0x200000,
	HAPP_RECONCILE_ONLYSTREAMS	= 0x1000000,
	HAPP_RECONCILE_NOSTREAMS	= 0x2000000,
	HAPP_RECONCILE_TEARSTREAMS	= 0x4000000
    } 	HAPP_RECONCILE_CODE;

typedef /* [public] */ 
enum __MIDL___MIDL_itf_happ_0129_0002
    {	HAPP_ENABLE_DEACTIVATE	= 0,
	HAPP_ENABLE_ACTIVATE	= 1,
	HAPP_ENABLE_NOT_APPLICABLE	= 2
    } 	HAPP_ENABLE_CODE;

typedef /* [public] */ 
enum __MIDL___MIDL_itf_happ_0129_0003
    {	HASEL_UNDEFINED	= 0xffffffff,
	HASEL_CHNODE	= 0,
	HASEL_SELECTION	= 1,
	HASEL_LABEL	= 2,
	HASEL_LABELNODE	= 3,
	HASEL_VOLATILECHNODE	= 4,
	HASEL_HEADERLABEL	= 90,
	HASEL_LBTU_UNITS	= 96,
	HASEL_LBTU_BASIS	= 97,
	HASEL_LBTU_UNITS_BASIS	= 98,
	HASEL_UNDERLYINGNODE	= 99,
	HASEL_SELECT_INDIRECT	= 100
    } 	HASELTYPE;

typedef /* [public][public][public] */ 
enum __MIDL___MIDL_itf_happ_0136_0001
    {	IAP_CONF_UNKNOWN	= 0xffffffff,
	IAP_CONF_NOACTION	= 0,
	IAP_CONF_REPLACE	= 2,
	IAP_CONF_MERGE	= 3,
	IAP_CONF_HIDE	= 4
    } 	IAP_CONFLICT_OPTION;


EXTERN_C const IID LIBID_Happ;

#ifndef __IHNode_INTERFACE_DEFINED__
#define __IHNode_INTERFACE_DEFINED__

/* interface IHNode */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHNode;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("8E567522-F9BA-11CF-90B2-0000C0A810C4")
    IHNode : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Application( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parent( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Dimension( 
            /* [retval][out] */ long *Dimension) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ValueType( 
            /* [retval][out] */ short *type) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Value( 
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Value( 
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetValueAndUnit( 
            /* [in] */ VARIANT Value,
            /* [in] */ short unitcol,
            /* [optional][in] */ VARIANT force) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetValueUnitAndBasis( 
            /* [in] */ VARIANT Value,
            /* [in] */ short unitcol,
            /* [in] */ BSTR basis,
            /* [optional][in] */ VARIANT force) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasAttribute( 
            /* [in] */ short attrnumber,
            /* [retval][out] */ VARIANT_BOOL *hasattr) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AttributeType( 
            /* [in] */ short attrnumber,
            /* [retval][out] */ short *type) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AttributeValue( 
            /* [in] */ short attrnumber,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AttributeValue( 
            /* [in] */ short attrnumber,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasClassAttribute( 
            /* [in] */ BSTR classid,
            /* [retval][out] */ VARIANT_BOOL *hasattr) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ClassAttributeType( 
            /* [in] */ BSTR classid,
            /* [retval][out] */ short *type) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ClassAttributeValue( 
            /* [in] */ BSTR classid,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ClassAttributeValue( 
            /* [in] */ BSTR classid,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Elements( 
            /* [retval][out] */ IHNodeCol **retval) = 0;
        
        virtual /* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE get_Process( 
            /* [retval][out] */ long *lpid) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Delete( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddClassAttribute( 
            /* [in] */ BSTR classid,
            /* [in] */ short type) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DeleteClassAttribute( 
            /* [in] */ BSTR classid) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE Copy( void) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE CopyWithFormat( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE PrintUseful( 
            /* [in] */ short append,
            /* [in] */ BSTR filename) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FindNode( 
            /* [in] */ BSTR path,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE Paste( 
            /* [retval][out] */ VARIANT_BOOL *succeeded) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE PasteSpecial( 
            /* [retval][out] */ VARIANT_BOOL *succeeded) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IsLinked( 
            /* [retval][out] */ VARIANT_BOOL *IsLinked) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE RemoveLink( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NextIncomplete( 
            /* [optional][out] */ VARIANT *code,
            /* [retval][out] */ BSTR *path) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE BrowseNext( 
            /* [in] */ short direction,
            /* [in] */ short io,
            /* [retval][out] */ BSTR *path) = 0;
        
        virtual /* [restricted][id] */ HRESULT STDMETHODCALLTYPE Dummy( 
            /* [in] */ HAPAttributeNumber __MIDL_0010,
            /* [in] */ HAPCompStatusCode __MIDL_0011) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Hide( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reveal( 
            /* [defaultvalue][optional][in] */ BSTR Name = L"") = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE NotifyRunStep( void) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE HoldNotifies( 
            /* [in] */ VARIANT_BOOL hold) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IsValid( 
            /* [retval][out] */ VARIANT_BOOL *valid) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NewChild( 
            /* [out][in] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RenameChild( 
            /* [out][in] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IsSpecialType( 
            /* [in] */ HAPSpecialNodeType type,
            /* [retval][out] */ VARIANT_BOOL *istype) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE Reaquire( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UnitString( 
            /* [retval][out] */ BSTR *unit) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ValueForUnit( 
            /* [in] */ short unitrow,
            /* [in] */ short unitcol,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE PFSSelectModel( void) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE PFSClearSelection( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reconcile( 
            /* [in] */ long code) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NewID( 
            /* [out][in] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Export( 
            /* [defaultvalue][optional][in] */ BSTR filename = L"") = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE DoObjectVerb( 
            /* [in] */ long oleverb,
            /* [retval][out] */ short *succeeded) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE AppendTemplate( 
            /* [in] */ IUnknown *pUnkTempl,
            /* [optional][in] */ VARIANT flag) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE Import( 
            /* [defaultvalue][optional][in] */ BSTR filename = L"") = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHNodeVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHNode * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHNode * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHNode * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHNode * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHNode * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHNode * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHNode * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Application )( 
            IHNode * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parent )( 
            IHNode * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IHNode * This,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *Name);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IHNode * This,
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR Name);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Dimension )( 
            IHNode * This,
            /* [retval][out] */ long *Dimension);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ValueType )( 
            IHNode * This,
            /* [retval][out] */ short *type);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Value )( 
            IHNode * This,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Value )( 
            IHNode * This,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetValueAndUnit )( 
            IHNode * This,
            /* [in] */ VARIANT Value,
            /* [in] */ short unitcol,
            /* [optional][in] */ VARIANT force);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetValueUnitAndBasis )( 
            IHNode * This,
            /* [in] */ VARIANT Value,
            /* [in] */ short unitcol,
            /* [in] */ BSTR basis,
            /* [optional][in] */ VARIANT force);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasAttribute )( 
            IHNode * This,
            /* [in] */ short attrnumber,
            /* [retval][out] */ VARIANT_BOOL *hasattr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AttributeType )( 
            IHNode * This,
            /* [in] */ short attrnumber,
            /* [retval][out] */ short *type);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AttributeValue )( 
            IHNode * This,
            /* [in] */ short attrnumber,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AttributeValue )( 
            IHNode * This,
            /* [in] */ short attrnumber,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasClassAttribute )( 
            IHNode * This,
            /* [in] */ BSTR classid,
            /* [retval][out] */ VARIANT_BOOL *hasattr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ClassAttributeType )( 
            IHNode * This,
            /* [in] */ BSTR classid,
            /* [retval][out] */ short *type);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ClassAttributeValue )( 
            IHNode * This,
            /* [in] */ BSTR classid,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *Value);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ClassAttributeValue )( 
            IHNode * This,
            /* [in] */ BSTR classid,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT Value);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Elements )( 
            IHNode * This,
            /* [retval][out] */ IHNodeCol **retval);
        
        /* [helpstring][restricted][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Process )( 
            IHNode * This,
            /* [retval][out] */ long *lpid);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Delete )( 
            IHNode * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveAll )( 
            IHNode * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddClassAttribute )( 
            IHNode * This,
            /* [in] */ BSTR classid,
            /* [in] */ short type);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DeleteClassAttribute )( 
            IHNode * This,
            /* [in] */ BSTR classid);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Copy )( 
            IHNode * This);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *CopyWithFormat )( 
            IHNode * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *PrintUseful )( 
            IHNode * This,
            /* [in] */ short append,
            /* [in] */ BSTR filename);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FindNode )( 
            IHNode * This,
            /* [in] */ BSTR path,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Paste )( 
            IHNode * This,
            /* [retval][out] */ VARIANT_BOOL *succeeded);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *PasteSpecial )( 
            IHNode * This,
            /* [retval][out] */ VARIANT_BOOL *succeeded);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *IsLinked )( 
            IHNode * This,
            /* [retval][out] */ VARIANT_BOOL *IsLinked);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveLink )( 
            IHNode * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NextIncomplete )( 
            IHNode * This,
            /* [optional][out] */ VARIANT *code,
            /* [retval][out] */ BSTR *path);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *BrowseNext )( 
            IHNode * This,
            /* [in] */ short direction,
            /* [in] */ short io,
            /* [retval][out] */ BSTR *path);
        
        /* [restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Dummy )( 
            IHNode * This,
            /* [in] */ HAPAttributeNumber __MIDL_0010,
            /* [in] */ HAPCompStatusCode __MIDL_0011);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Hide )( 
            IHNode * This,
            /* [in] */ BSTR Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reveal )( 
            IHNode * This,
            /* [defaultvalue][optional][in] */ BSTR Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IHNode * This);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *NotifyRunStep )( 
            IHNode * This);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *HoldNotifies )( 
            IHNode * This,
            /* [in] */ VARIANT_BOOL hold);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *IsValid )( 
            IHNode * This,
            /* [retval][out] */ VARIANT_BOOL *valid);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NewChild )( 
            IHNode * This,
            /* [out][in] */ BSTR *Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RenameChild )( 
            IHNode * This,
            /* [out][in] */ BSTR *Name);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *IsSpecialType )( 
            IHNode * This,
            /* [in] */ HAPSpecialNodeType type,
            /* [retval][out] */ VARIANT_BOOL *istype);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Reaquire )( 
            IHNode * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UnitString )( 
            IHNode * This,
            /* [retval][out] */ BSTR *unit);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ValueForUnit )( 
            IHNode * This,
            /* [in] */ short unitrow,
            /* [in] */ short unitcol,
            /* [retval][out] */ VARIANT *Value);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *PFSSelectModel )( 
            IHNode * This);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *PFSClearSelection )( 
            IHNode * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reconcile )( 
            IHNode * This,
            /* [in] */ long code);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NewID )( 
            IHNode * This,
            /* [out][in] */ BSTR *Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Export )( 
            IHNode * This,
            /* [defaultvalue][optional][in] */ BSTR filename);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *DoObjectVerb )( 
            IHNode * This,
            /* [in] */ long oleverb,
            /* [retval][out] */ short *succeeded);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *AppendTemplate )( 
            IHNode * This,
            /* [in] */ IUnknown *pUnkTempl,
            /* [optional][in] */ VARIANT flag);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *Import )( 
            IHNode * This,
            /* [defaultvalue][optional][in] */ BSTR filename);
        
        END_INTERFACE
    } IHNodeVtbl;

    interface IHNode
    {
        CONST_VTBL struct IHNodeVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHNode_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHNode_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHNode_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHNode_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHNode_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHNode_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHNode_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHNode_get_Application(This,retval)	\
    (This)->lpVtbl -> get_Application(This,retval)

#define IHNode_get_Parent(This,retval)	\
    (This)->lpVtbl -> get_Parent(This,retval)

#define IHNode_get_Name(This,force,Name)	\
    (This)->lpVtbl -> get_Name(This,force,Name)

#define IHNode_put_Name(This,force,Name)	\
    (This)->lpVtbl -> put_Name(This,force,Name)

#define IHNode_get_Dimension(This,Dimension)	\
    (This)->lpVtbl -> get_Dimension(This,Dimension)

#define IHNode_get_ValueType(This,type)	\
    (This)->lpVtbl -> get_ValueType(This,type)

#define IHNode_get_Value(This,force,Value)	\
    (This)->lpVtbl -> get_Value(This,force,Value)

#define IHNode_put_Value(This,force,Value)	\
    (This)->lpVtbl -> put_Value(This,force,Value)

#define IHNode_SetValueAndUnit(This,Value,unitcol,force)	\
    (This)->lpVtbl -> SetValueAndUnit(This,Value,unitcol,force)

#define IHNode_SetValueUnitAndBasis(This,Value,unitcol,basis,force)	\
    (This)->lpVtbl -> SetValueUnitAndBasis(This,Value,unitcol,basis,force)

#define IHNode_get_HasAttribute(This,attrnumber,hasattr)	\
    (This)->lpVtbl -> get_HasAttribute(This,attrnumber,hasattr)

#define IHNode_get_AttributeType(This,attrnumber,type)	\
    (This)->lpVtbl -> get_AttributeType(This,attrnumber,type)

#define IHNode_get_AttributeValue(This,attrnumber,force,Value)	\
    (This)->lpVtbl -> get_AttributeValue(This,attrnumber,force,Value)

#define IHNode_put_AttributeValue(This,attrnumber,force,Value)	\
    (This)->lpVtbl -> put_AttributeValue(This,attrnumber,force,Value)

#define IHNode_get_HasClassAttribute(This,classid,hasattr)	\
    (This)->lpVtbl -> get_HasClassAttribute(This,classid,hasattr)

#define IHNode_get_ClassAttributeType(This,classid,type)	\
    (This)->lpVtbl -> get_ClassAttributeType(This,classid,type)

#define IHNode_get_ClassAttributeValue(This,classid,force,Value)	\
    (This)->lpVtbl -> get_ClassAttributeValue(This,classid,force,Value)

#define IHNode_put_ClassAttributeValue(This,classid,force,Value)	\
    (This)->lpVtbl -> put_ClassAttributeValue(This,classid,force,Value)

#define IHNode_get_Elements(This,retval)	\
    (This)->lpVtbl -> get_Elements(This,retval)

#define IHNode_get_Process(This,lpid)	\
    (This)->lpVtbl -> get_Process(This,lpid)

#define IHNode_Delete(This)	\
    (This)->lpVtbl -> Delete(This)

#define IHNode_RemoveAll(This)	\
    (This)->lpVtbl -> RemoveAll(This)

#define IHNode_AddClassAttribute(This,classid,type)	\
    (This)->lpVtbl -> AddClassAttribute(This,classid,type)

#define IHNode_DeleteClassAttribute(This,classid)	\
    (This)->lpVtbl -> DeleteClassAttribute(This,classid)

#define IHNode_Copy(This)	\
    (This)->lpVtbl -> Copy(This)

#define IHNode_CopyWithFormat(This)	\
    (This)->lpVtbl -> CopyWithFormat(This)

#define IHNode_PrintUseful(This,append,filename)	\
    (This)->lpVtbl -> PrintUseful(This,append,filename)

#define IHNode_FindNode(This,path,retval)	\
    (This)->lpVtbl -> FindNode(This,path,retval)

#define IHNode_Paste(This,succeeded)	\
    (This)->lpVtbl -> Paste(This,succeeded)

#define IHNode_PasteSpecial(This,succeeded)	\
    (This)->lpVtbl -> PasteSpecial(This,succeeded)

#define IHNode_IsLinked(This,IsLinked)	\
    (This)->lpVtbl -> IsLinked(This,IsLinked)

#define IHNode_RemoveLink(This)	\
    (This)->lpVtbl -> RemoveLink(This)

#define IHNode_NextIncomplete(This,code,path)	\
    (This)->lpVtbl -> NextIncomplete(This,code,path)

#define IHNode_BrowseNext(This,direction,io,path)	\
    (This)->lpVtbl -> BrowseNext(This,direction,io,path)

#define IHNode_Dummy(This,__MIDL_0010,__MIDL_0011)	\
    (This)->lpVtbl -> Dummy(This,__MIDL_0010,__MIDL_0011)

#define IHNode_Hide(This,Name)	\
    (This)->lpVtbl -> Hide(This,Name)

#define IHNode_Reveal(This,Name)	\
    (This)->lpVtbl -> Reveal(This,Name)

#define IHNode_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IHNode_NotifyRunStep(This)	\
    (This)->lpVtbl -> NotifyRunStep(This)

#define IHNode_HoldNotifies(This,hold)	\
    (This)->lpVtbl -> HoldNotifies(This,hold)

#define IHNode_IsValid(This,valid)	\
    (This)->lpVtbl -> IsValid(This,valid)

#define IHNode_NewChild(This,Name)	\
    (This)->lpVtbl -> NewChild(This,Name)

#define IHNode_RenameChild(This,Name)	\
    (This)->lpVtbl -> RenameChild(This,Name)

#define IHNode_IsSpecialType(This,type,istype)	\
    (This)->lpVtbl -> IsSpecialType(This,type,istype)

#define IHNode_Reaquire(This)	\
    (This)->lpVtbl -> Reaquire(This)

#define IHNode_get_UnitString(This,unit)	\
    (This)->lpVtbl -> get_UnitString(This,unit)

#define IHNode_get_ValueForUnit(This,unitrow,unitcol,Value)	\
    (This)->lpVtbl -> get_ValueForUnit(This,unitrow,unitcol,Value)

#define IHNode_PFSSelectModel(This)	\
    (This)->lpVtbl -> PFSSelectModel(This)

#define IHNode_PFSClearSelection(This)	\
    (This)->lpVtbl -> PFSClearSelection(This)

#define IHNode_Reconcile(This,code)	\
    (This)->lpVtbl -> Reconcile(This,code)

#define IHNode_NewID(This,Name)	\
    (This)->lpVtbl -> NewID(This,Name)

#define IHNode_Export(This,filename)	\
    (This)->lpVtbl -> Export(This,filename)

#define IHNode_DoObjectVerb(This,oleverb,succeeded)	\
    (This)->lpVtbl -> DoObjectVerb(This,oleverb,succeeded)

#define IHNode_AppendTemplate(This,pUnkTempl,flag)	\
    (This)->lpVtbl -> AppendTemplate(This,pUnkTempl,flag)

#define IHNode_Import(This,filename)	\
    (This)->lpVtbl -> Import(This,filename)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Application_Proxy( 
    IHNode * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHNode_get_Application_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Parent_Proxy( 
    IHNode * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHNode_get_Parent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Name_Proxy( 
    IHNode * This,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IHNode_get_Name_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNode_put_Name_Proxy( 
    IHNode * This,
    /* [optional][in] */ VARIANT force,
    /* [in] */ BSTR Name);


void __RPC_STUB IHNode_put_Name_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Dimension_Proxy( 
    IHNode * This,
    /* [retval][out] */ long *Dimension);


void __RPC_STUB IHNode_get_Dimension_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_ValueType_Proxy( 
    IHNode * This,
    /* [retval][out] */ short *type);


void __RPC_STUB IHNode_get_ValueType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Value_Proxy( 
    IHNode * This,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ VARIANT *Value);


void __RPC_STUB IHNode_get_Value_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNode_put_Value_Proxy( 
    IHNode * This,
    /* [optional][in] */ VARIANT force,
    /* [in] */ VARIANT Value);


void __RPC_STUB IHNode_put_Value_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_SetValueAndUnit_Proxy( 
    IHNode * This,
    /* [in] */ VARIANT Value,
    /* [in] */ short unitcol,
    /* [optional][in] */ VARIANT force);


void __RPC_STUB IHNode_SetValueAndUnit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_SetValueUnitAndBasis_Proxy( 
    IHNode * This,
    /* [in] */ VARIANT Value,
    /* [in] */ short unitcol,
    /* [in] */ BSTR basis,
    /* [optional][in] */ VARIANT force);


void __RPC_STUB IHNode_SetValueUnitAndBasis_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_HasAttribute_Proxy( 
    IHNode * This,
    /* [in] */ short attrnumber,
    /* [retval][out] */ VARIANT_BOOL *hasattr);


void __RPC_STUB IHNode_get_HasAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_AttributeType_Proxy( 
    IHNode * This,
    /* [in] */ short attrnumber,
    /* [retval][out] */ short *type);


void __RPC_STUB IHNode_get_AttributeType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_AttributeValue_Proxy( 
    IHNode * This,
    /* [in] */ short attrnumber,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ VARIANT *Value);


void __RPC_STUB IHNode_get_AttributeValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNode_put_AttributeValue_Proxy( 
    IHNode * This,
    /* [in] */ short attrnumber,
    /* [optional][in] */ VARIANT force,
    /* [in] */ VARIANT Value);


void __RPC_STUB IHNode_put_AttributeValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_HasClassAttribute_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid,
    /* [retval][out] */ VARIANT_BOOL *hasattr);


void __RPC_STUB IHNode_get_HasClassAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_ClassAttributeType_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid,
    /* [retval][out] */ short *type);


void __RPC_STUB IHNode_get_ClassAttributeType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_ClassAttributeValue_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ VARIANT *Value);


void __RPC_STUB IHNode_get_ClassAttributeValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNode_put_ClassAttributeValue_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid,
    /* [optional][in] */ VARIANT force,
    /* [in] */ VARIANT Value);


void __RPC_STUB IHNode_put_ClassAttributeValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Elements_Proxy( 
    IHNode * This,
    /* [retval][out] */ IHNodeCol **retval);


void __RPC_STUB IHNode_get_Elements_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_Process_Proxy( 
    IHNode * This,
    /* [retval][out] */ long *lpid);


void __RPC_STUB IHNode_get_Process_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Delete_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_Delete_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_RemoveAll_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_RemoveAll_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_AddClassAttribute_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid,
    /* [in] */ short type);


void __RPC_STUB IHNode_AddClassAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_DeleteClassAttribute_Proxy( 
    IHNode * This,
    /* [in] */ BSTR classid);


void __RPC_STUB IHNode_DeleteClassAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_Copy_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_Copy_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_CopyWithFormat_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_CopyWithFormat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_PrintUseful_Proxy( 
    IHNode * This,
    /* [in] */ short append,
    /* [in] */ BSTR filename);


void __RPC_STUB IHNode_PrintUseful_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_FindNode_Proxy( 
    IHNode * This,
    /* [in] */ BSTR path,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHNode_FindNode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_Paste_Proxy( 
    IHNode * This,
    /* [retval][out] */ VARIANT_BOOL *succeeded);


void __RPC_STUB IHNode_Paste_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_PasteSpecial_Proxy( 
    IHNode * This,
    /* [retval][out] */ VARIANT_BOOL *succeeded);


void __RPC_STUB IHNode_PasteSpecial_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_IsLinked_Proxy( 
    IHNode * This,
    /* [retval][out] */ VARIANT_BOOL *IsLinked);


void __RPC_STUB IHNode_IsLinked_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_RemoveLink_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_RemoveLink_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_NextIncomplete_Proxy( 
    IHNode * This,
    /* [optional][out] */ VARIANT *code,
    /* [retval][out] */ BSTR *path);


void __RPC_STUB IHNode_NextIncomplete_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_BrowseNext_Proxy( 
    IHNode * This,
    /* [in] */ short direction,
    /* [in] */ short io,
    /* [retval][out] */ BSTR *path);


void __RPC_STUB IHNode_BrowseNext_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_Dummy_Proxy( 
    IHNode * This,
    /* [in] */ HAPAttributeNumber __MIDL_0010,
    /* [in] */ HAPCompStatusCode __MIDL_0011);


void __RPC_STUB IHNode_Dummy_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Hide_Proxy( 
    IHNode * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IHNode_Hide_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Reveal_Proxy( 
    IHNode * This,
    /* [defaultvalue][optional][in] */ BSTR Name);


void __RPC_STUB IHNode_Reveal_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Clear_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_NotifyRunStep_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_NotifyRunStep_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_HoldNotifies_Proxy( 
    IHNode * This,
    /* [in] */ VARIANT_BOOL hold);


void __RPC_STUB IHNode_HoldNotifies_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_IsValid_Proxy( 
    IHNode * This,
    /* [retval][out] */ VARIANT_BOOL *valid);


void __RPC_STUB IHNode_IsValid_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_NewChild_Proxy( 
    IHNode * This,
    /* [out][in] */ BSTR *Name);


void __RPC_STUB IHNode_NewChild_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_RenameChild_Proxy( 
    IHNode * This,
    /* [out][in] */ BSTR *Name);


void __RPC_STUB IHNode_RenameChild_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_IsSpecialType_Proxy( 
    IHNode * This,
    /* [in] */ HAPSpecialNodeType type,
    /* [retval][out] */ VARIANT_BOOL *istype);


void __RPC_STUB IHNode_IsSpecialType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_Reaquire_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_Reaquire_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_UnitString_Proxy( 
    IHNode * This,
    /* [retval][out] */ BSTR *unit);


void __RPC_STUB IHNode_get_UnitString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNode_get_ValueForUnit_Proxy( 
    IHNode * This,
    /* [in] */ short unitrow,
    /* [in] */ short unitcol,
    /* [retval][out] */ VARIANT *Value);


void __RPC_STUB IHNode_get_ValueForUnit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_PFSSelectModel_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_PFSSelectModel_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_PFSClearSelection_Proxy( 
    IHNode * This);


void __RPC_STUB IHNode_PFSClearSelection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Reconcile_Proxy( 
    IHNode * This,
    /* [in] */ long code);


void __RPC_STUB IHNode_Reconcile_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_NewID_Proxy( 
    IHNode * This,
    /* [out][in] */ BSTR *Name);


void __RPC_STUB IHNode_NewID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNode_Export_Proxy( 
    IHNode * This,
    /* [defaultvalue][optional][in] */ BSTR filename);


void __RPC_STUB IHNode_Export_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_DoObjectVerb_Proxy( 
    IHNode * This,
    /* [in] */ long oleverb,
    /* [retval][out] */ short *succeeded);


void __RPC_STUB IHNode_DoObjectVerb_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHNode_AppendTemplate_Proxy( 
    IHNode * This,
    /* [in] */ IUnknown *pUnkTempl,
    /* [optional][in] */ VARIANT flag);


void __RPC_STUB IHNode_AppendTemplate_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHNode_Import_Proxy( 
    IHNode * This,
    /* [defaultvalue][optional][in] */ BSTR filename);


void __RPC_STUB IHNode_Import_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHNode_INTERFACE_DEFINED__ */


#ifndef __IHapp_INTERFACE_DEFINED__
#define __IHapp_INTERFACE_DEFINED__

/* interface IHapp */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHapp;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("8E567521-F9BA-11CF-90B2-0000C0A810C4")
    IHapp : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Application( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parent( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FullName( 
            /* [retval][out] */ BSTR *retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Visible( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Visible( 
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [helpstring][hidden][propget][id] */ HRESULT STDMETHODCALLTYPE get_New( 
            /* [optional][in] */ VARIANT filename,
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE Restore( 
            /* [in] */ BSTR filename) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE InitNew( 
            /* [optional][in] */ VARIANT filename,
            /* [optional][in] */ VARIANT overwrite) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE InitFromFile( 
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT readonly) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE InitFromArchive( 
            /* [in] */ BSTR *filename) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tree( 
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Save( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveAs( 
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT overwrite) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE WriteArchive( 
            /* [in] */ BSTR *filename) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE Run( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( 
            /* [optional][in] */ VARIANT reserved) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Choose( 
            /* [out][in] */ short *flag,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NewSelection( 
            /* [in] */ BSTR Key,
            /* [retval][out] */ IHSelection **buffer) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Selection( 
            /* [in] */ BSTR Key,
            /* [retval][out] */ IHSelection **buffer) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DeleteSelection( 
            /* [in] */ BSTR Key) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveSelection( 
            /* [in] */ BSTR Key) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reinit( void) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE InitFromTemplate( 
            /* [in] */ BSTR *filename) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CreateRouteTree( 
            /* [in] */ BSTR *propname,
            /* [in] */ BSTR *routeid,
            /* [in] */ BSTR *opsetid,
            /* [in] */ short flag,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE get_NewAsync( 
            /* [optional][in] */ VARIANT filename,
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE SetAsync( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Engine( 
            /* [retval][out] */ IHAPEngine **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveLink( 
            /* [in] */ IStream *pStrm,
            /* [in] */ long format) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE LoadLink( 
            /* [in] */ IStream *pStrm,
            /* [in] */ long format) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE RefreshExportedLinks( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Activate( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetCompat( 
            /* [in] */ int flag) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reconcile( 
            /* [in] */ long code) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AdviseParent( 
            long dAdviseType,
            long lParam) = 0;
        
        virtual /* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE get_New2( 
            /* [optional][in] */ VARIANT filename,
            /* [defaultvalue][optional][in] */ long bSync,
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Generate( 
            /* [in] */ BSTR filename,
            /* [defaultvalue][optional][in] */ int mode = 0) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Readback( 
            /* [in] */ BSTR filename,
            /* [defaultvalue][optional][in] */ int mode = 0) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetParent( 
            /* [in] */ IParentAdviseSink *pParentAdviseSink,
            /* [in] */ long dwCookie) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE WriteArchive2( 
            /* [in] */ BSTR *filename,
            /* [in] */ long bSaveChildren) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_New3( 
            /* [optional][in] */ VARIANT filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode,
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Restore2( 
            /* [in] */ BSTR filename,
            /* [retval][out] */ VARIANT *vRes) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitNew2( 
            /* [optional][in] */ VARIANT notused,
            /* [optional][in] */ VARIANT notused2,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitFromFile2( 
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT readonly,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitFromArchive2( 
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Run2( 
            /* [optional][in] */ VARIANT async) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitFromTemplate2( 
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LibRef( 
            /* [retval][out] */ IHAPLibRef **LibRef) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SuppressDialogs( 
            /* [retval][out] */ long *bSuppress) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SuppressDialogs( 
            /* [in] */ long bSuppress) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Export( 
            /* [in] */ HAPEXPType reptype,
            /* [in] */ BSTR filename) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE UIDisable( 
            /* [in] */ BSTR Key) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EngineSimulation( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EngineServer( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitFromXML( 
            /* [in] */ BSTR argument) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHappVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHapp * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHapp * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHapp * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHapp * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHapp * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHapp * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHapp * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Application )( 
            IHapp * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parent )( 
            IHapp * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FullName )( 
            IHapp * This,
            /* [retval][out] */ BSTR *retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IHapp * This,
            /* [retval][out] */ BSTR *retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Visible )( 
            IHapp * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Visible )( 
            IHapp * This,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [helpstring][hidden][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_New )( 
            IHapp * This,
            /* [optional][in] */ VARIANT filename,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *Restore )( 
            IHapp * This,
            /* [in] */ BSTR filename);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *InitNew )( 
            IHapp * This,
            /* [optional][in] */ VARIANT filename,
            /* [optional][in] */ VARIANT overwrite);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromFile )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT readonly);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromArchive )( 
            IHapp * This,
            /* [in] */ BSTR *filename);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tree )( 
            IHapp * This,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Save )( 
            IHapp * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveAs )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT overwrite);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *WriteArchive )( 
            IHapp * This,
            /* [in] */ BSTR *filename);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *Run )( 
            IHapp * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IHapp * This,
            /* [optional][in] */ VARIANT reserved);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Choose )( 
            IHapp * This,
            /* [out][in] */ short *flag,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NewSelection )( 
            IHapp * This,
            /* [in] */ BSTR Key,
            /* [retval][out] */ IHSelection **buffer);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Selection )( 
            IHapp * This,
            /* [in] */ BSTR Key,
            /* [retval][out] */ IHSelection **buffer);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DeleteSelection )( 
            IHapp * This,
            /* [in] */ BSTR Key);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveSelection )( 
            IHapp * This,
            /* [in] */ BSTR Key);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reinit )( 
            IHapp * This);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromTemplate )( 
            IHapp * This,
            /* [in] */ BSTR *filename);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CreateRouteTree )( 
            IHapp * This,
            /* [in] */ BSTR *propname,
            /* [in] */ BSTR *routeid,
            /* [in] */ BSTR *opsetid,
            /* [in] */ short flag,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][restricted][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NewAsync )( 
            IHapp * This,
            /* [optional][in] */ VARIANT filename,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *SetAsync )( 
            IHapp * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Engine )( 
            IHapp * This,
            /* [retval][out] */ IHAPEngine **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveLink )( 
            IHapp * This,
            /* [in] */ IStream *pStrm,
            /* [in] */ long format);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *LoadLink )( 
            IHapp * This,
            /* [in] */ IStream *pStrm,
            /* [in] */ long format);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *RefreshExportedLinks )( 
            IHapp * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Activate )( 
            IHapp * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetCompat )( 
            IHapp * This,
            /* [in] */ int flag);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reconcile )( 
            IHapp * This,
            /* [in] */ long code);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AdviseParent )( 
            IHapp * This,
            long dAdviseType,
            long lParam);
        
        /* [helpstring][restricted][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_New2 )( 
            IHapp * This,
            /* [optional][in] */ VARIANT filename,
            /* [defaultvalue][optional][in] */ long bSync,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Generate )( 
            IHapp * This,
            /* [in] */ BSTR filename,
            /* [defaultvalue][optional][in] */ int mode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Readback )( 
            IHapp * This,
            /* [in] */ BSTR filename,
            /* [defaultvalue][optional][in] */ int mode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetParent )( 
            IHapp * This,
            /* [in] */ IParentAdviseSink *pParentAdviseSink,
            /* [in] */ long dwCookie);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *WriteArchive2 )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [in] */ long bSaveChildren);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_New3 )( 
            IHapp * This,
            /* [optional][in] */ VARIANT filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Restore2 )( 
            IHapp * This,
            /* [in] */ BSTR filename,
            /* [retval][out] */ VARIANT *vRes);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitNew2 )( 
            IHapp * This,
            /* [optional][in] */ VARIANT notused,
            /* [optional][in] */ VARIANT notused2,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromFile2 )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT readonly,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromArchive2 )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Run2 )( 
            IHapp * This,
            /* [optional][in] */ VARIANT async);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromTemplate2 )( 
            IHapp * This,
            /* [in] */ BSTR *filename,
            /* [optional][in] */ VARIANT host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [optional][in] */ VARIANT failmode);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LibRef )( 
            IHapp * This,
            /* [retval][out] */ IHAPLibRef **LibRef);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SuppressDialogs )( 
            IHapp * This,
            /* [retval][out] */ long *bSuppress);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SuppressDialogs )( 
            IHapp * This,
            /* [in] */ long bSuppress);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Export )( 
            IHapp * This,
            /* [in] */ HAPEXPType reptype,
            /* [in] */ BSTR filename);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *UIDisable )( 
            IHapp * This,
            /* [in] */ BSTR Key);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EngineSimulation )( 
            IHapp * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EngineServer )( 
            IHapp * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitFromXML )( 
            IHapp * This,
            /* [in] */ BSTR argument);
        
        END_INTERFACE
    } IHappVtbl;

    interface IHapp
    {
        CONST_VTBL struct IHappVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHapp_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHapp_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHapp_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHapp_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHapp_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHapp_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHapp_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHapp_get_Application(This,retval)	\
    (This)->lpVtbl -> get_Application(This,retval)

#define IHapp_get_Parent(This,retval)	\
    (This)->lpVtbl -> get_Parent(This,retval)

#define IHapp_get_FullName(This,retval)	\
    (This)->lpVtbl -> get_FullName(This,retval)

#define IHapp_get_Name(This,retval)	\
    (This)->lpVtbl -> get_Name(This,retval)

#define IHapp_get_Visible(This,retval)	\
    (This)->lpVtbl -> get_Visible(This,retval)

#define IHapp_put_Visible(This,retval)	\
    (This)->lpVtbl -> put_Visible(This,retval)

#define IHapp_get_New(This,filename,retval)	\
    (This)->lpVtbl -> get_New(This,filename,retval)

#define IHapp_Restore(This,filename)	\
    (This)->lpVtbl -> Restore(This,filename)

#define IHapp_InitNew(This,filename,overwrite)	\
    (This)->lpVtbl -> InitNew(This,filename,overwrite)

#define IHapp_InitFromFile(This,filename,readonly)	\
    (This)->lpVtbl -> InitFromFile(This,filename,readonly)

#define IHapp_InitFromArchive(This,filename)	\
    (This)->lpVtbl -> InitFromArchive(This,filename)

#define IHapp_get_Tree(This,retval)	\
    (This)->lpVtbl -> get_Tree(This,retval)

#define IHapp_Save(This)	\
    (This)->lpVtbl -> Save(This)

#define IHapp_SaveAs(This,filename,overwrite)	\
    (This)->lpVtbl -> SaveAs(This,filename,overwrite)

#define IHapp_WriteArchive(This,filename)	\
    (This)->lpVtbl -> WriteArchive(This,filename)

#define IHapp_Run(This)	\
    (This)->lpVtbl -> Run(This)

#define IHapp_Close(This,reserved)	\
    (This)->lpVtbl -> Close(This,reserved)

#define IHapp_get_Choose(This,flag,retval)	\
    (This)->lpVtbl -> get_Choose(This,flag,retval)

#define IHapp_NewSelection(This,Key,buffer)	\
    (This)->lpVtbl -> NewSelection(This,Key,buffer)

#define IHapp_get_Selection(This,Key,buffer)	\
    (This)->lpVtbl -> get_Selection(This,Key,buffer)

#define IHapp_DeleteSelection(This,Key)	\
    (This)->lpVtbl -> DeleteSelection(This,Key)

#define IHapp_SaveSelection(This,Key)	\
    (This)->lpVtbl -> SaveSelection(This,Key)

#define IHapp_Reinit(This)	\
    (This)->lpVtbl -> Reinit(This)

#define IHapp_InitFromTemplate(This,filename)	\
    (This)->lpVtbl -> InitFromTemplate(This,filename)

#define IHapp_CreateRouteTree(This,propname,routeid,opsetid,flag,retval)	\
    (This)->lpVtbl -> CreateRouteTree(This,propname,routeid,opsetid,flag,retval)

#define IHapp_get_NewAsync(This,filename,retval)	\
    (This)->lpVtbl -> get_NewAsync(This,filename,retval)

#define IHapp_SetAsync(This)	\
    (This)->lpVtbl -> SetAsync(This)

#define IHapp_get_Engine(This,retval)	\
    (This)->lpVtbl -> get_Engine(This,retval)

#define IHapp_SaveLink(This,pStrm,format)	\
    (This)->lpVtbl -> SaveLink(This,pStrm,format)

#define IHapp_LoadLink(This,pStrm,format)	\
    (This)->lpVtbl -> LoadLink(This,pStrm,format)

#define IHapp_RefreshExportedLinks(This)	\
    (This)->lpVtbl -> RefreshExportedLinks(This)

#define IHapp_Activate(This)	\
    (This)->lpVtbl -> Activate(This)

#define IHapp_SetCompat(This,flag)	\
    (This)->lpVtbl -> SetCompat(This,flag)

#define IHapp_Reconcile(This,code)	\
    (This)->lpVtbl -> Reconcile(This,code)

#define IHapp_AdviseParent(This,dAdviseType,lParam)	\
    (This)->lpVtbl -> AdviseParent(This,dAdviseType,lParam)

#define IHapp_get_New2(This,filename,bSync,retval)	\
    (This)->lpVtbl -> get_New2(This,filename,bSync,retval)

#define IHapp_Generate(This,filename,mode)	\
    (This)->lpVtbl -> Generate(This,filename,mode)

#define IHapp_Readback(This,filename,mode)	\
    (This)->lpVtbl -> Readback(This,filename,mode)

#define IHapp_SetParent(This,pParentAdviseSink,dwCookie)	\
    (This)->lpVtbl -> SetParent(This,pParentAdviseSink,dwCookie)

#define IHapp_WriteArchive2(This,filename,bSaveChildren)	\
    (This)->lpVtbl -> WriteArchive2(This,filename,bSaveChildren)

#define IHapp_get_New3(This,filename,host_type,node,username,password,working_directory,failmode,retval)	\
    (This)->lpVtbl -> get_New3(This,filename,host_type,node,username,password,working_directory,failmode,retval)

#define IHapp_get_Restore2(This,filename,vRes)	\
    (This)->lpVtbl -> get_Restore2(This,filename,vRes)

#define IHapp_InitNew2(This,notused,notused2,host_type,node,username,password,working_directory,failmode)	\
    (This)->lpVtbl -> InitNew2(This,notused,notused2,host_type,node,username,password,working_directory,failmode)

#define IHapp_InitFromFile2(This,filename,readonly,host_type,node,username,password,working_directory,failmode)	\
    (This)->lpVtbl -> InitFromFile2(This,filename,readonly,host_type,node,username,password,working_directory,failmode)

#define IHapp_InitFromArchive2(This,filename,host_type,node,username,password,working_directory,failmode)	\
    (This)->lpVtbl -> InitFromArchive2(This,filename,host_type,node,username,password,working_directory,failmode)

#define IHapp_Run2(This,async)	\
    (This)->lpVtbl -> Run2(This,async)

#define IHapp_InitFromTemplate2(This,filename,host_type,node,username,password,working_directory,failmode)	\
    (This)->lpVtbl -> InitFromTemplate2(This,filename,host_type,node,username,password,working_directory,failmode)

#define IHapp_get_LibRef(This,LibRef)	\
    (This)->lpVtbl -> get_LibRef(This,LibRef)

#define IHapp_get_SuppressDialogs(This,bSuppress)	\
    (This)->lpVtbl -> get_SuppressDialogs(This,bSuppress)

#define IHapp_put_SuppressDialogs(This,bSuppress)	\
    (This)->lpVtbl -> put_SuppressDialogs(This,bSuppress)

#define IHapp_Export(This,reptype,filename)	\
    (This)->lpVtbl -> Export(This,reptype,filename)

#define IHapp_UIDisable(This,Key)	\
    (This)->lpVtbl -> UIDisable(This,Key)

#define IHapp_get_EngineSimulation(This,retval)	\
    (This)->lpVtbl -> get_EngineSimulation(This,retval)

#define IHapp_get_EngineServer(This,retval)	\
    (This)->lpVtbl -> get_EngineServer(This,retval)

#define IHapp_InitFromXML(This,argument)	\
    (This)->lpVtbl -> InitFromXML(This,argument)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Application_Proxy( 
    IHapp * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_Application_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Parent_Proxy( 
    IHapp * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_Parent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_FullName_Proxy( 
    IHapp * This,
    /* [retval][out] */ BSTR *retval);


void __RPC_STUB IHapp_get_FullName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Name_Proxy( 
    IHapp * This,
    /* [retval][out] */ BSTR *retval);


void __RPC_STUB IHapp_get_Name_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Visible_Proxy( 
    IHapp * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHapp_get_Visible_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHapp_put_Visible_Proxy( 
    IHapp * This,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IHapp_put_Visible_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_New_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT filename,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_New_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_Restore_Proxy( 
    IHapp * This,
    /* [in] */ BSTR filename);


void __RPC_STUB IHapp_Restore_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitNew_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT filename,
    /* [optional][in] */ VARIANT overwrite);


void __RPC_STUB IHapp_InitNew_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromFile_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [optional][in] */ VARIANT readonly);


void __RPC_STUB IHapp_InitFromFile_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromArchive_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename);


void __RPC_STUB IHapp_InitFromArchive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Tree_Proxy( 
    IHapp * This,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHapp_get_Tree_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Save_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_Save_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_SaveAs_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [optional][in] */ VARIANT overwrite);


void __RPC_STUB IHapp_SaveAs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_WriteArchive_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename);


void __RPC_STUB IHapp_WriteArchive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_Run_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_Run_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Close_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT reserved);


void __RPC_STUB IHapp_Close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Choose_Proxy( 
    IHapp * This,
    /* [out][in] */ short *flag,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHapp_get_Choose_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_NewSelection_Proxy( 
    IHapp * This,
    /* [in] */ BSTR Key,
    /* [retval][out] */ IHSelection **buffer);


void __RPC_STUB IHapp_NewSelection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Selection_Proxy( 
    IHapp * This,
    /* [in] */ BSTR Key,
    /* [retval][out] */ IHSelection **buffer);


void __RPC_STUB IHapp_get_Selection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_DeleteSelection_Proxy( 
    IHapp * This,
    /* [in] */ BSTR Key);


void __RPC_STUB IHapp_DeleteSelection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_SaveSelection_Proxy( 
    IHapp * This,
    /* [in] */ BSTR Key);


void __RPC_STUB IHapp_SaveSelection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Reinit_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_Reinit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromTemplate_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename);


void __RPC_STUB IHapp_InitFromTemplate_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_CreateRouteTree_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *propname,
    /* [in] */ BSTR *routeid,
    /* [in] */ BSTR *opsetid,
    /* [in] */ short flag,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHapp_CreateRouteTree_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_NewAsync_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT filename,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_NewAsync_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHapp_SetAsync_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_SetAsync_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Engine_Proxy( 
    IHapp * This,
    /* [retval][out] */ IHAPEngine **retval);


void __RPC_STUB IHapp_get_Engine_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_SaveLink_Proxy( 
    IHapp * This,
    /* [in] */ IStream *pStrm,
    /* [in] */ long format);


void __RPC_STUB IHapp_SaveLink_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_LoadLink_Proxy( 
    IHapp * This,
    /* [in] */ IStream *pStrm,
    /* [in] */ long format);


void __RPC_STUB IHapp_LoadLink_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHapp_RefreshExportedLinks_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_RefreshExportedLinks_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Activate_Proxy( 
    IHapp * This);


void __RPC_STUB IHapp_Activate_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_SetCompat_Proxy( 
    IHapp * This,
    /* [in] */ int flag);


void __RPC_STUB IHapp_SetCompat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Reconcile_Proxy( 
    IHapp * This,
    /* [in] */ long code);


void __RPC_STUB IHapp_Reconcile_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_AdviseParent_Proxy( 
    IHapp * This,
    long dAdviseType,
    long lParam);


void __RPC_STUB IHapp_AdviseParent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_New2_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT filename,
    /* [defaultvalue][optional][in] */ long bSync,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_New2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Generate_Proxy( 
    IHapp * This,
    /* [in] */ BSTR filename,
    /* [defaultvalue][optional][in] */ int mode);


void __RPC_STUB IHapp_Generate_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Readback_Proxy( 
    IHapp * This,
    /* [in] */ BSTR filename,
    /* [defaultvalue][optional][in] */ int mode);


void __RPC_STUB IHapp_Readback_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_SetParent_Proxy( 
    IHapp * This,
    /* [in] */ IParentAdviseSink *pParentAdviseSink,
    /* [in] */ long dwCookie);


void __RPC_STUB IHapp_SetParent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_WriteArchive2_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [in] */ long bSaveChildren);


void __RPC_STUB IHapp_WriteArchive2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_New3_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT filename,
    /* [optional][in] */ VARIANT host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [optional][in] */ VARIANT failmode,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHapp_get_New3_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_Restore2_Proxy( 
    IHapp * This,
    /* [in] */ BSTR filename,
    /* [retval][out] */ VARIANT *vRes);


void __RPC_STUB IHapp_get_Restore2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitNew2_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT notused,
    /* [optional][in] */ VARIANT notused2,
    /* [optional][in] */ VARIANT host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [optional][in] */ VARIANT failmode);


void __RPC_STUB IHapp_InitNew2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromFile2_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [optional][in] */ VARIANT readonly,
    /* [optional][in] */ VARIANT host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [optional][in] */ VARIANT failmode);


void __RPC_STUB IHapp_InitFromFile2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromArchive2_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [optional][in] */ VARIANT host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [optional][in] */ VARIANT failmode);


void __RPC_STUB IHapp_InitFromArchive2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Run2_Proxy( 
    IHapp * This,
    /* [optional][in] */ VARIANT async);


void __RPC_STUB IHapp_Run2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromTemplate2_Proxy( 
    IHapp * This,
    /* [in] */ BSTR *filename,
    /* [optional][in] */ VARIANT host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [optional][in] */ VARIANT failmode);


void __RPC_STUB IHapp_InitFromTemplate2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_LibRef_Proxy( 
    IHapp * This,
    /* [retval][out] */ IHAPLibRef **LibRef);


void __RPC_STUB IHapp_get_LibRef_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_SuppressDialogs_Proxy( 
    IHapp * This,
    /* [retval][out] */ long *bSuppress);


void __RPC_STUB IHapp_get_SuppressDialogs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHapp_put_SuppressDialogs_Proxy( 
    IHapp * This,
    /* [in] */ long bSuppress);


void __RPC_STUB IHapp_put_SuppressDialogs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_Export_Proxy( 
    IHapp * This,
    /* [in] */ HAPEXPType reptype,
    /* [in] */ BSTR filename);


void __RPC_STUB IHapp_Export_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_UIDisable_Proxy( 
    IHapp * This,
    /* [in] */ BSTR Key);


void __RPC_STUB IHapp_UIDisable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_EngineSimulation_Proxy( 
    IHapp * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IHapp_get_EngineSimulation_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHapp_get_EngineServer_Proxy( 
    IHapp * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IHapp_get_EngineServer_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHapp_InitFromXML_Proxy( 
    IHapp * This,
    /* [in] */ BSTR argument);


void __RPC_STUB IHapp_InitFromXML_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHapp_INTERFACE_DEFINED__ */


#ifndef __IHSelection_INTERFACE_DEFINED__
#define __IHSelection_INTERFACE_DEFINED__

/* interface IHSelection */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHSelection;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("71F9FD70-9ED4-11D0-9475-0000C07972E4")
    IHSelection : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Application( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parent( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *len) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Item( 
            /* [in] */ VARIANT loc_or_name,
            /* [retval][out] */ VARIANT *Item) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ItemType( 
            /* [in] */ VARIANT loc_or_name,
            /* [retval][out] */ short *type) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Key( 
            /* [retval][out] */ BSTR *keystring) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Label( 
            /* [in] */ long location,
            /* [retval][out] */ BSTR *Label) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Label( 
            /* [in] */ long location,
            /* [in] */ BSTR Label) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Add( 
            /* [in] */ VARIANT loc_or_name,
            /* [in] */ short type,
            /* [in] */ VARIANT Item,
            /* [in] */ IDispatch *owner,
            /* [optional][in] */ VARIANT scrollarea,
            /* [optional][in] */ VARIANT row,
            /* [optional][in] */ VARIANT col,
            /* [optional][in] */ VARIANT index,
            /* [optional][in] */ VARIANT xtwip,
            /* [optional][in] */ VARIANT ytwip) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Remove( 
            /* [in] */ VARIANT loc_or_name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Find( 
            /* [in] */ VARIANT object,
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Copy( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CopyWithFormat( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Paste( 
            /* [retval][out] */ VARIANT_BOOL *succeeded) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE PasteSpecial( 
            /* [optional][in] */ VARIANT format,
            /* [optional][in] */ VARIANT link,
            /* [retval][out] */ VARIANT_BOOL *succeeded) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Lock( 
            /* [in] */ VARIANT_BOOL fLock) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ClearOwned( 
            /* [in] */ IDispatch *owner) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CanPrint( 
            /* [retval][out] */ VARIANT_BOOL *print) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE PreparePrinting( 
            /* [retval][out] */ VARIANT *hglb) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHSelectionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHSelection * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHSelection * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHSelection * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHSelection * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHSelection * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHSelection * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHSelection * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Application )( 
            IHSelection * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parent )( 
            IHSelection * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IHSelection * This,
            /* [retval][out] */ long *len);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Item )( 
            IHSelection * This,
            /* [in] */ VARIANT loc_or_name,
            /* [retval][out] */ VARIANT *Item);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ItemType )( 
            IHSelection * This,
            /* [in] */ VARIANT loc_or_name,
            /* [retval][out] */ short *type);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Key )( 
            IHSelection * This,
            /* [retval][out] */ BSTR *keystring);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Label )( 
            IHSelection * This,
            /* [in] */ long location,
            /* [retval][out] */ BSTR *Label);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Label )( 
            IHSelection * This,
            /* [in] */ long location,
            /* [in] */ BSTR Label);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Add )( 
            IHSelection * This,
            /* [in] */ VARIANT loc_or_name,
            /* [in] */ short type,
            /* [in] */ VARIANT Item,
            /* [in] */ IDispatch *owner,
            /* [optional][in] */ VARIANT scrollarea,
            /* [optional][in] */ VARIANT row,
            /* [optional][in] */ VARIANT col,
            /* [optional][in] */ VARIANT index,
            /* [optional][in] */ VARIANT xtwip,
            /* [optional][in] */ VARIANT ytwip);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IHSelection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Remove )( 
            IHSelection * This,
            /* [in] */ VARIANT loc_or_name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Find )( 
            IHSelection * This,
            /* [in] */ VARIANT object,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Copy )( 
            IHSelection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CopyWithFormat )( 
            IHSelection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Paste )( 
            IHSelection * This,
            /* [retval][out] */ VARIANT_BOOL *succeeded);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *PasteSpecial )( 
            IHSelection * This,
            /* [optional][in] */ VARIANT format,
            /* [optional][in] */ VARIANT link,
            /* [retval][out] */ VARIANT_BOOL *succeeded);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Lock )( 
            IHSelection * This,
            /* [in] */ VARIANT_BOOL fLock);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ClearOwned )( 
            IHSelection * This,
            /* [in] */ IDispatch *owner);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CanPrint )( 
            IHSelection * This,
            /* [retval][out] */ VARIANT_BOOL *print);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *PreparePrinting )( 
            IHSelection * This,
            /* [retval][out] */ VARIANT *hglb);
        
        END_INTERFACE
    } IHSelectionVtbl;

    interface IHSelection
    {
        CONST_VTBL struct IHSelectionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHSelection_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHSelection_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHSelection_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHSelection_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHSelection_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHSelection_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHSelection_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHSelection_get_Application(This,retval)	\
    (This)->lpVtbl -> get_Application(This,retval)

#define IHSelection_get_Parent(This,retval)	\
    (This)->lpVtbl -> get_Parent(This,retval)

#define IHSelection_get_Count(This,len)	\
    (This)->lpVtbl -> get_Count(This,len)

#define IHSelection_get_Item(This,loc_or_name,Item)	\
    (This)->lpVtbl -> get_Item(This,loc_or_name,Item)

#define IHSelection_get_ItemType(This,loc_or_name,type)	\
    (This)->lpVtbl -> get_ItemType(This,loc_or_name,type)

#define IHSelection_get_Key(This,keystring)	\
    (This)->lpVtbl -> get_Key(This,keystring)

#define IHSelection_get_Label(This,location,Label)	\
    (This)->lpVtbl -> get_Label(This,location,Label)

#define IHSelection_put_Label(This,location,Label)	\
    (This)->lpVtbl -> put_Label(This,location,Label)

#define IHSelection_Add(This,loc_or_name,type,Item,owner,scrollarea,row,col,index,xtwip,ytwip)	\
    (This)->lpVtbl -> Add(This,loc_or_name,type,Item,owner,scrollarea,row,col,index,xtwip,ytwip)

#define IHSelection_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IHSelection_Remove(This,loc_or_name)	\
    (This)->lpVtbl -> Remove(This,loc_or_name)

#define IHSelection_Find(This,object,retval)	\
    (This)->lpVtbl -> Find(This,object,retval)

#define IHSelection_Copy(This)	\
    (This)->lpVtbl -> Copy(This)

#define IHSelection_CopyWithFormat(This)	\
    (This)->lpVtbl -> CopyWithFormat(This)

#define IHSelection_Paste(This,succeeded)	\
    (This)->lpVtbl -> Paste(This,succeeded)

#define IHSelection_PasteSpecial(This,format,link,succeeded)	\
    (This)->lpVtbl -> PasteSpecial(This,format,link,succeeded)

#define IHSelection_Lock(This,fLock)	\
    (This)->lpVtbl -> Lock(This,fLock)

#define IHSelection_ClearOwned(This,owner)	\
    (This)->lpVtbl -> ClearOwned(This,owner)

#define IHSelection_CanPrint(This,print)	\
    (This)->lpVtbl -> CanPrint(This,print)

#define IHSelection_PreparePrinting(This,hglb)	\
    (This)->lpVtbl -> PreparePrinting(This,hglb)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Application_Proxy( 
    IHSelection * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHSelection_get_Application_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Parent_Proxy( 
    IHSelection * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHSelection_get_Parent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Count_Proxy( 
    IHSelection * This,
    /* [retval][out] */ long *len);


void __RPC_STUB IHSelection_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Item_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT loc_or_name,
    /* [retval][out] */ VARIANT *Item);


void __RPC_STUB IHSelection_get_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_ItemType_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT loc_or_name,
    /* [retval][out] */ short *type);


void __RPC_STUB IHSelection_get_ItemType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Key_Proxy( 
    IHSelection * This,
    /* [retval][out] */ BSTR *keystring);


void __RPC_STUB IHSelection_get_Key_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHSelection_get_Label_Proxy( 
    IHSelection * This,
    /* [in] */ long location,
    /* [retval][out] */ BSTR *Label);


void __RPC_STUB IHSelection_get_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHSelection_put_Label_Proxy( 
    IHSelection * This,
    /* [in] */ long location,
    /* [in] */ BSTR Label);


void __RPC_STUB IHSelection_put_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Add_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT loc_or_name,
    /* [in] */ short type,
    /* [in] */ VARIANT Item,
    /* [in] */ IDispatch *owner,
    /* [optional][in] */ VARIANT scrollarea,
    /* [optional][in] */ VARIANT row,
    /* [optional][in] */ VARIANT col,
    /* [optional][in] */ VARIANT index,
    /* [optional][in] */ VARIANT xtwip,
    /* [optional][in] */ VARIANT ytwip);


void __RPC_STUB IHSelection_Add_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Clear_Proxy( 
    IHSelection * This);


void __RPC_STUB IHSelection_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Remove_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT loc_or_name);


void __RPC_STUB IHSelection_Remove_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Find_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT object,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHSelection_Find_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Copy_Proxy( 
    IHSelection * This);


void __RPC_STUB IHSelection_Copy_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_CopyWithFormat_Proxy( 
    IHSelection * This);


void __RPC_STUB IHSelection_CopyWithFormat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Paste_Proxy( 
    IHSelection * This,
    /* [retval][out] */ VARIANT_BOOL *succeeded);


void __RPC_STUB IHSelection_Paste_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_PasteSpecial_Proxy( 
    IHSelection * This,
    /* [optional][in] */ VARIANT format,
    /* [optional][in] */ VARIANT link,
    /* [retval][out] */ VARIANT_BOOL *succeeded);


void __RPC_STUB IHSelection_PasteSpecial_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_Lock_Proxy( 
    IHSelection * This,
    /* [in] */ VARIANT_BOOL fLock);


void __RPC_STUB IHSelection_Lock_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_ClearOwned_Proxy( 
    IHSelection * This,
    /* [in] */ IDispatch *owner);


void __RPC_STUB IHSelection_ClearOwned_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_CanPrint_Proxy( 
    IHSelection * This,
    /* [retval][out] */ VARIANT_BOOL *print);


void __RPC_STUB IHSelection_CanPrint_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHSelection_PreparePrinting_Proxy( 
    IHSelection * This,
    /* [retval][out] */ VARIANT *hglb);


void __RPC_STUB IHSelection_PreparePrinting_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHSelection_INTERFACE_DEFINED__ */


#ifndef __IHAPEngine_INTERFACE_DEFINED__
#define __IHAPEngine_INTERFACE_DEFINED__

/* interface IHAPEngine */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHAPEngine;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("472C18A0-2603-11D1-9B33-0000C07EE8F2")
    IHAPEngine : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RunControl( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RunControl( 
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlPanel( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlPanel( 
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Ready( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE Run( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Step( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Stop( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reinit( 
            /* [optional][in] */ VARIANT object_type,
            /* [optional][in] */ VARIANT object_id) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE MoveTo( 
            /* [in] */ IAP_MOVETO_TYPE object_type,
            /* [optional][in] */ VARIANT object_id) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ConnectionDialog( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Host( 
            /* [in] */ int host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [retval][out] */ VARIANT_BOOL *succeeded) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HostCount( 
            /* [retval][out] */ int *Count) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE HostDescription( 
            /* [in] */ int host_type,
            /* [retval][out] */ BSTR *description) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RunSettings( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EngineFilesSettings( 
            /* [in] */ IAP_ENGINEFILES file,
            /* [retval][out] */ BSTR *filename) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EngineFilesSettings( 
            /* [in] */ IAP_ENGINEFILES file,
            /* [in] */ BSTR filename) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OptionSettings( 
            /* [in] */ IAP_RUN_OPTION type,
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_OptionSettings( 
            /* [in] */ IAP_RUN_OPTION type,
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE StopPoints( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_StopPointCount( 
            /* [retval][out] */ long *Count) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetStopPoint( 
            /* [in] */ long index,
            /* [out] */ IAP_STOPPOINT_TYPE *type,
            /* [out] */ BSTR *object_id,
            /* [out] */ int *before_or_after) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddStopPoint( 
            /* [in] */ IAP_STOPPOINT_TYPE type,
            /* [in] */ BSTR object_id,
            /* [in] */ int before_or_after) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DeleteStopPoint( 
            /* [in] */ long index) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ClearStopPoints( void) = 0;
        
        virtual /* [restricted][id] */ HRESULT STDMETHODCALLTYPE Dummy( 
            /* [in] */ IAP_REINIT_TYPE rtype) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsRunning( 
            /* [retval][out] */ VARIANT_BOOL *bRunning) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Run2( 
            /* [optional][in] */ VARIANT async) = 0;
        
        virtual /* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE ProcessInput( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ExportReport( 
            /* [in] */ BSTR filename,
            /* [in] */ HAPReportType contents,
            /* [optional][in] */ VARIANT object_id) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SynchronizeEO( 
            /* [optional][in] */ VARIANT reserved) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ReinitializeEO( 
            /* [optional][in] */ VARIANT reserved) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHAPEngineVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHAPEngine * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHAPEngine * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHAPEngine * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHAPEngine * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHAPEngine * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHAPEngine * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHAPEngine * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RunControl )( 
            IHAPEngine * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RunControl )( 
            IHAPEngine * This,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlPanel )( 
            IHAPEngine * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlPanel )( 
            IHAPEngine * This,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Ready )( 
            IHAPEngine * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *Run )( 
            IHAPEngine * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Step )( 
            IHAPEngine * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Stop )( 
            IHAPEngine * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reinit )( 
            IHAPEngine * This,
            /* [optional][in] */ VARIANT object_type,
            /* [optional][in] */ VARIANT object_id);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *MoveTo )( 
            IHAPEngine * This,
            /* [in] */ IAP_MOVETO_TYPE object_type,
            /* [optional][in] */ VARIANT object_id);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ConnectionDialog )( 
            IHAPEngine * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Host )( 
            IHAPEngine * This,
            /* [in] */ int host_type,
            /* [optional][in] */ VARIANT node,
            /* [optional][in] */ VARIANT username,
            /* [optional][in] */ VARIANT password,
            /* [optional][in] */ VARIANT working_directory,
            /* [retval][out] */ VARIANT_BOOL *succeeded);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HostCount )( 
            IHAPEngine * This,
            /* [retval][out] */ int *Count);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *HostDescription )( 
            IHAPEngine * This,
            /* [in] */ int host_type,
            /* [retval][out] */ BSTR *description);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RunSettings )( 
            IHAPEngine * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EngineFilesSettings )( 
            IHAPEngine * This,
            /* [in] */ IAP_ENGINEFILES file,
            /* [retval][out] */ BSTR *filename);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EngineFilesSettings )( 
            IHAPEngine * This,
            /* [in] */ IAP_ENGINEFILES file,
            /* [in] */ BSTR filename);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OptionSettings )( 
            IHAPEngine * This,
            /* [in] */ IAP_RUN_OPTION type,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_OptionSettings )( 
            IHAPEngine * This,
            /* [in] */ IAP_RUN_OPTION type,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *StopPoints )( 
            IHAPEngine * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_StopPointCount )( 
            IHAPEngine * This,
            /* [retval][out] */ long *Count);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetStopPoint )( 
            IHAPEngine * This,
            /* [in] */ long index,
            /* [out] */ IAP_STOPPOINT_TYPE *type,
            /* [out] */ BSTR *object_id,
            /* [out] */ int *before_or_after);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddStopPoint )( 
            IHAPEngine * This,
            /* [in] */ IAP_STOPPOINT_TYPE type,
            /* [in] */ BSTR object_id,
            /* [in] */ int before_or_after);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DeleteStopPoint )( 
            IHAPEngine * This,
            /* [in] */ long index);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ClearStopPoints )( 
            IHAPEngine * This);
        
        /* [restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Dummy )( 
            IHAPEngine * This,
            /* [in] */ IAP_REINIT_TYPE rtype);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsRunning )( 
            IHAPEngine * This,
            /* [retval][out] */ VARIANT_BOOL *bRunning);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Run2 )( 
            IHAPEngine * This,
            /* [optional][in] */ VARIANT async);
        
        /* [helpstring][hidden][id] */ HRESULT ( STDMETHODCALLTYPE *ProcessInput )( 
            IHAPEngine * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ExportReport )( 
            IHAPEngine * This,
            /* [in] */ BSTR filename,
            /* [in] */ HAPReportType contents,
            /* [optional][in] */ VARIANT object_id);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SynchronizeEO )( 
            IHAPEngine * This,
            /* [optional][in] */ VARIANT reserved);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ReinitializeEO )( 
            IHAPEngine * This,
            /* [optional][in] */ VARIANT reserved);
        
        END_INTERFACE
    } IHAPEngineVtbl;

    interface IHAPEngine
    {
        CONST_VTBL struct IHAPEngineVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHAPEngine_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHAPEngine_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHAPEngine_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHAPEngine_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHAPEngine_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHAPEngine_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHAPEngine_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHAPEngine_get_RunControl(This,retval)	\
    (This)->lpVtbl -> get_RunControl(This,retval)

#define IHAPEngine_put_RunControl(This,retval)	\
    (This)->lpVtbl -> put_RunControl(This,retval)

#define IHAPEngine_get_ControlPanel(This,retval)	\
    (This)->lpVtbl -> get_ControlPanel(This,retval)

#define IHAPEngine_put_ControlPanel(This,retval)	\
    (This)->lpVtbl -> put_ControlPanel(This,retval)

#define IHAPEngine_get_Ready(This,retval)	\
    (This)->lpVtbl -> get_Ready(This,retval)

#define IHAPEngine_Run(This)	\
    (This)->lpVtbl -> Run(This)

#define IHAPEngine_Step(This)	\
    (This)->lpVtbl -> Step(This)

#define IHAPEngine_Stop(This)	\
    (This)->lpVtbl -> Stop(This)

#define IHAPEngine_Reinit(This,object_type,object_id)	\
    (This)->lpVtbl -> Reinit(This,object_type,object_id)

#define IHAPEngine_MoveTo(This,object_type,object_id)	\
    (This)->lpVtbl -> MoveTo(This,object_type,object_id)

#define IHAPEngine_ConnectionDialog(This)	\
    (This)->lpVtbl -> ConnectionDialog(This)

#define IHAPEngine_Host(This,host_type,node,username,password,working_directory,succeeded)	\
    (This)->lpVtbl -> Host(This,host_type,node,username,password,working_directory,succeeded)

#define IHAPEngine_get_HostCount(This,Count)	\
    (This)->lpVtbl -> get_HostCount(This,Count)

#define IHAPEngine_HostDescription(This,host_type,description)	\
    (This)->lpVtbl -> HostDescription(This,host_type,description)

#define IHAPEngine_RunSettings(This)	\
    (This)->lpVtbl -> RunSettings(This)

#define IHAPEngine_get_EngineFilesSettings(This,file,filename)	\
    (This)->lpVtbl -> get_EngineFilesSettings(This,file,filename)

#define IHAPEngine_put_EngineFilesSettings(This,file,filename)	\
    (This)->lpVtbl -> put_EngineFilesSettings(This,file,filename)

#define IHAPEngine_get_OptionSettings(This,type,retval)	\
    (This)->lpVtbl -> get_OptionSettings(This,type,retval)

#define IHAPEngine_put_OptionSettings(This,type,retval)	\
    (This)->lpVtbl -> put_OptionSettings(This,type,retval)

#define IHAPEngine_StopPoints(This)	\
    (This)->lpVtbl -> StopPoints(This)

#define IHAPEngine_get_StopPointCount(This,Count)	\
    (This)->lpVtbl -> get_StopPointCount(This,Count)

#define IHAPEngine_GetStopPoint(This,index,type,object_id,before_or_after)	\
    (This)->lpVtbl -> GetStopPoint(This,index,type,object_id,before_or_after)

#define IHAPEngine_AddStopPoint(This,type,object_id,before_or_after)	\
    (This)->lpVtbl -> AddStopPoint(This,type,object_id,before_or_after)

#define IHAPEngine_DeleteStopPoint(This,index)	\
    (This)->lpVtbl -> DeleteStopPoint(This,index)

#define IHAPEngine_ClearStopPoints(This)	\
    (This)->lpVtbl -> ClearStopPoints(This)

#define IHAPEngine_Dummy(This,rtype)	\
    (This)->lpVtbl -> Dummy(This,rtype)

#define IHAPEngine_get_IsRunning(This,bRunning)	\
    (This)->lpVtbl -> get_IsRunning(This,bRunning)

#define IHAPEngine_Run2(This,async)	\
    (This)->lpVtbl -> Run2(This,async)

#define IHAPEngine_ProcessInput(This)	\
    (This)->lpVtbl -> ProcessInput(This)

#define IHAPEngine_ExportReport(This,filename,contents,object_id)	\
    (This)->lpVtbl -> ExportReport(This,filename,contents,object_id)

#define IHAPEngine_SynchronizeEO(This,reserved)	\
    (This)->lpVtbl -> SynchronizeEO(This,reserved)

#define IHAPEngine_ReinitializeEO(This,reserved)	\
    (This)->lpVtbl -> ReinitializeEO(This,reserved)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_RunControl_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHAPEngine_get_RunControl_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_put_RunControl_Proxy( 
    IHAPEngine * This,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IHAPEngine_put_RunControl_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_ControlPanel_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHAPEngine_get_ControlPanel_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_put_ControlPanel_Proxy( 
    IHAPEngine * This,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IHAPEngine_put_ControlPanel_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_Ready_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHAPEngine_get_Ready_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Run_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_Run_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Step_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_Step_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Stop_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_Stop_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Reinit_Proxy( 
    IHAPEngine * This,
    /* [optional][in] */ VARIANT object_type,
    /* [optional][in] */ VARIANT object_id);


void __RPC_STUB IHAPEngine_Reinit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_MoveTo_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_MOVETO_TYPE object_type,
    /* [optional][in] */ VARIANT object_id);


void __RPC_STUB IHAPEngine_MoveTo_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_ConnectionDialog_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_ConnectionDialog_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Host_Proxy( 
    IHAPEngine * This,
    /* [in] */ int host_type,
    /* [optional][in] */ VARIANT node,
    /* [optional][in] */ VARIANT username,
    /* [optional][in] */ VARIANT password,
    /* [optional][in] */ VARIANT working_directory,
    /* [retval][out] */ VARIANT_BOOL *succeeded);


void __RPC_STUB IHAPEngine_Host_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_HostCount_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ int *Count);


void __RPC_STUB IHAPEngine_get_HostCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_HostDescription_Proxy( 
    IHAPEngine * This,
    /* [in] */ int host_type,
    /* [retval][out] */ BSTR *description);


void __RPC_STUB IHAPEngine_HostDescription_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_RunSettings_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_RunSettings_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_EngineFilesSettings_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_ENGINEFILES file,
    /* [retval][out] */ BSTR *filename);


void __RPC_STUB IHAPEngine_get_EngineFilesSettings_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_put_EngineFilesSettings_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_ENGINEFILES file,
    /* [in] */ BSTR filename);


void __RPC_STUB IHAPEngine_put_EngineFilesSettings_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_OptionSettings_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_RUN_OPTION type,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IHAPEngine_get_OptionSettings_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_put_OptionSettings_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_RUN_OPTION type,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IHAPEngine_put_OptionSettings_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_StopPoints_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_StopPoints_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_StopPointCount_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ long *Count);


void __RPC_STUB IHAPEngine_get_StopPointCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_GetStopPoint_Proxy( 
    IHAPEngine * This,
    /* [in] */ long index,
    /* [out] */ IAP_STOPPOINT_TYPE *type,
    /* [out] */ BSTR *object_id,
    /* [out] */ int *before_or_after);


void __RPC_STUB IHAPEngine_GetStopPoint_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_AddStopPoint_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_STOPPOINT_TYPE type,
    /* [in] */ BSTR object_id,
    /* [in] */ int before_or_after);


void __RPC_STUB IHAPEngine_AddStopPoint_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_DeleteStopPoint_Proxy( 
    IHAPEngine * This,
    /* [in] */ long index);


void __RPC_STUB IHAPEngine_DeleteStopPoint_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_ClearStopPoints_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_ClearStopPoints_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [restricted][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Dummy_Proxy( 
    IHAPEngine * This,
    /* [in] */ IAP_REINIT_TYPE rtype);


void __RPC_STUB IHAPEngine_Dummy_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_get_IsRunning_Proxy( 
    IHAPEngine * This,
    /* [retval][out] */ VARIANT_BOOL *bRunning);


void __RPC_STUB IHAPEngine_get_IsRunning_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_Run2_Proxy( 
    IHAPEngine * This,
    /* [optional][in] */ VARIANT async);


void __RPC_STUB IHAPEngine_Run2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][hidden][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_ProcessInput_Proxy( 
    IHAPEngine * This);


void __RPC_STUB IHAPEngine_ProcessInput_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_ExportReport_Proxy( 
    IHAPEngine * This,
    /* [in] */ BSTR filename,
    /* [in] */ HAPReportType contents,
    /* [optional][in] */ VARIANT object_id);


void __RPC_STUB IHAPEngine_ExportReport_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_SynchronizeEO_Proxy( 
    IHAPEngine * This,
    /* [optional][in] */ VARIANT reserved);


void __RPC_STUB IHAPEngine_SynchronizeEO_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPEngine_ReinitializeEO_Proxy( 
    IHAPEngine * This,
    /* [optional][in] */ VARIANT reserved);


void __RPC_STUB IHAPEngine_ReinitializeEO_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHAPEngine_INTERFACE_DEFINED__ */


#ifndef __IParentAdviseSink_INTERFACE_DEFINED__
#define __IParentAdviseSink_INTERFACE_DEFINED__

/* interface IParentAdviseSink */
/* [object][oleautomation][helpstring][uuid] */ 


EXTERN_C const IID IID_IParentAdviseSink;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("503FD631-F63E-11D1-8D3E-0000C033EAF2")
    IParentAdviseSink : public IUnknown
    {
    public:
        virtual HRESULT __stdcall OnDataChange( 
            /* [in] */ long dwCookie,
            /* [in] */ long nSeverity) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IParentAdviseSinkVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IParentAdviseSink * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IParentAdviseSink * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IParentAdviseSink * This);
        
        HRESULT ( __stdcall *OnDataChange )( 
            IParentAdviseSink * This,
            /* [in] */ long dwCookie,
            /* [in] */ long nSeverity);
        
        END_INTERFACE
    } IParentAdviseSinkVtbl;

    interface IParentAdviseSink
    {
        CONST_VTBL struct IParentAdviseSinkVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IParentAdviseSink_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IParentAdviseSink_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IParentAdviseSink_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IParentAdviseSink_OnDataChange(This,dwCookie,nSeverity)	\
    (This)->lpVtbl -> OnDataChange(This,dwCookie,nSeverity)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT __stdcall IParentAdviseSink_OnDataChange_Proxy( 
    IParentAdviseSink * This,
    /* [in] */ long dwCookie,
    /* [in] */ long nSeverity);


void __RPC_STUB IParentAdviseSink_OnDataChange_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IParentAdviseSink_INTERFACE_DEFINED__ */


#ifndef __IHAPLibRef_INTERFACE_DEFINED__
#define __IHAPLibRef_INTERFACE_DEFINED__

/* interface IHAPLibRef */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHAPLibRef;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("8E567525-F9BA-11CF-90B2-0000C0A810C4")
    IHAPLibRef : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CountLibs( 
            /* [retval][out] */ short *Count) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LibraryName( 
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libname) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LibraryPath( 
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libpath) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InsertLibrary( 
            /* [in] */ BSTR path,
            /* [in] */ short location,
            /* [retval][out] */ BSTR *displayname) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveLibrary( 
            /* [in] */ short location) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE MoveLibrary( 
            /* [in] */ short fromloc,
            /* [in] */ short toloc) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CategoryName( 
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libname) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CategorySelected( 
            /* [in] */ BSTR Name,
            /* [retval][out] */ short *selected) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CategorySelected( 
            /* [in] */ BSTR Name,
            /* [in] */ short selected) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CategoryLocSelected( 
            /* [in] */ short index,
            /* [retval][out] */ short *selected) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CategoryLocSelected( 
            /* [in] */ short index,
            /* [in] */ short selected) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE MoveCategory( 
            /* [in] */ short fromloc,
            /* [in] */ short toloc) = 0;
        
        virtual /* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE Enum( 
            /* [out] */ IEnumAModelLibrary **ppEnum) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetLibraryActive( 
            /* [in] */ BSTR displayname) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHAPLibRefVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHAPLibRef * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHAPLibRef * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHAPLibRef * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHAPLibRef * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHAPLibRef * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHAPLibRef * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHAPLibRef * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CountLibs )( 
            IHAPLibRef * This,
            /* [retval][out] */ short *Count);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LibraryName )( 
            IHAPLibRef * This,
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libname);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LibraryPath )( 
            IHAPLibRef * This,
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libpath);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InsertLibrary )( 
            IHAPLibRef * This,
            /* [in] */ BSTR path,
            /* [in] */ short location,
            /* [retval][out] */ BSTR *displayname);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveLibrary )( 
            IHAPLibRef * This,
            /* [in] */ short location);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *MoveLibrary )( 
            IHAPLibRef * This,
            /* [in] */ short fromloc,
            /* [in] */ short toloc);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CategoryName )( 
            IHAPLibRef * This,
            /* [in] */ short index,
            /* [retval][out] */ BSTR *libname);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CategorySelected )( 
            IHAPLibRef * This,
            /* [in] */ BSTR Name,
            /* [retval][out] */ short *selected);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CategorySelected )( 
            IHAPLibRef * This,
            /* [in] */ BSTR Name,
            /* [in] */ short selected);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CategoryLocSelected )( 
            IHAPLibRef * This,
            /* [in] */ short index,
            /* [retval][out] */ short *selected);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CategoryLocSelected )( 
            IHAPLibRef * This,
            /* [in] */ short index,
            /* [in] */ short selected);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *MoveCategory )( 
            IHAPLibRef * This,
            /* [in] */ short fromloc,
            /* [in] */ short toloc);
        
        /* [helpstring][restricted][id] */ HRESULT ( STDMETHODCALLTYPE *Enum )( 
            IHAPLibRef * This,
            /* [out] */ IEnumAModelLibrary **ppEnum);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetLibraryActive )( 
            IHAPLibRef * This,
            /* [in] */ BSTR displayname);
        
        END_INTERFACE
    } IHAPLibRefVtbl;

    interface IHAPLibRef
    {
        CONST_VTBL struct IHAPLibRefVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHAPLibRef_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHAPLibRef_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHAPLibRef_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHAPLibRef_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHAPLibRef_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHAPLibRef_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHAPLibRef_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHAPLibRef_get_CountLibs(This,Count)	\
    (This)->lpVtbl -> get_CountLibs(This,Count)

#define IHAPLibRef_get_LibraryName(This,index,libname)	\
    (This)->lpVtbl -> get_LibraryName(This,index,libname)

#define IHAPLibRef_get_LibraryPath(This,index,libpath)	\
    (This)->lpVtbl -> get_LibraryPath(This,index,libpath)

#define IHAPLibRef_InsertLibrary(This,path,location,displayname)	\
    (This)->lpVtbl -> InsertLibrary(This,path,location,displayname)

#define IHAPLibRef_RemoveLibrary(This,location)	\
    (This)->lpVtbl -> RemoveLibrary(This,location)

#define IHAPLibRef_MoveLibrary(This,fromloc,toloc)	\
    (This)->lpVtbl -> MoveLibrary(This,fromloc,toloc)

#define IHAPLibRef_get_CategoryName(This,index,libname)	\
    (This)->lpVtbl -> get_CategoryName(This,index,libname)

#define IHAPLibRef_get_CategorySelected(This,Name,selected)	\
    (This)->lpVtbl -> get_CategorySelected(This,Name,selected)

#define IHAPLibRef_put_CategorySelected(This,Name,selected)	\
    (This)->lpVtbl -> put_CategorySelected(This,Name,selected)

#define IHAPLibRef_get_CategoryLocSelected(This,index,selected)	\
    (This)->lpVtbl -> get_CategoryLocSelected(This,index,selected)

#define IHAPLibRef_put_CategoryLocSelected(This,index,selected)	\
    (This)->lpVtbl -> put_CategoryLocSelected(This,index,selected)

#define IHAPLibRef_MoveCategory(This,fromloc,toloc)	\
    (This)->lpVtbl -> MoveCategory(This,fromloc,toloc)

#define IHAPLibRef_Enum(This,ppEnum)	\
    (This)->lpVtbl -> Enum(This,ppEnum)

#define IHAPLibRef_SetLibraryActive(This,displayname)	\
    (This)->lpVtbl -> SetLibraryActive(This,displayname)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_CountLibs_Proxy( 
    IHAPLibRef * This,
    /* [retval][out] */ short *Count);


void __RPC_STUB IHAPLibRef_get_CountLibs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_LibraryName_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short index,
    /* [retval][out] */ BSTR *libname);


void __RPC_STUB IHAPLibRef_get_LibraryName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_LibraryPath_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short index,
    /* [retval][out] */ BSTR *libpath);


void __RPC_STUB IHAPLibRef_get_LibraryPath_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_InsertLibrary_Proxy( 
    IHAPLibRef * This,
    /* [in] */ BSTR path,
    /* [in] */ short location,
    /* [retval][out] */ BSTR *displayname);


void __RPC_STUB IHAPLibRef_InsertLibrary_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_RemoveLibrary_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short location);


void __RPC_STUB IHAPLibRef_RemoveLibrary_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_MoveLibrary_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short fromloc,
    /* [in] */ short toloc);


void __RPC_STUB IHAPLibRef_MoveLibrary_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_CategoryName_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short index,
    /* [retval][out] */ BSTR *libname);


void __RPC_STUB IHAPLibRef_get_CategoryName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_CategorySelected_Proxy( 
    IHAPLibRef * This,
    /* [in] */ BSTR Name,
    /* [retval][out] */ short *selected);


void __RPC_STUB IHAPLibRef_get_CategorySelected_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_put_CategorySelected_Proxy( 
    IHAPLibRef * This,
    /* [in] */ BSTR Name,
    /* [in] */ short selected);


void __RPC_STUB IHAPLibRef_put_CategorySelected_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_get_CategoryLocSelected_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short index,
    /* [retval][out] */ short *selected);


void __RPC_STUB IHAPLibRef_get_CategoryLocSelected_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_put_CategoryLocSelected_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short index,
    /* [in] */ short selected);


void __RPC_STUB IHAPLibRef_put_CategoryLocSelected_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_MoveCategory_Proxy( 
    IHAPLibRef * This,
    /* [in] */ short fromloc,
    /* [in] */ short toloc);


void __RPC_STUB IHAPLibRef_MoveCategory_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][restricted][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_Enum_Proxy( 
    IHAPLibRef * This,
    /* [out] */ IEnumAModelLibrary **ppEnum);


void __RPC_STUB IHAPLibRef_Enum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAPLibRef_SetLibraryActive_Proxy( 
    IHAPLibRef * This,
    /* [in] */ BSTR displayname);


void __RPC_STUB IHAPLibRef_SetLibraryActive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHAPLibRef_INTERFACE_DEFINED__ */


#ifndef __IEnumAModelLibrary_INTERFACE_DEFINED__
#define __IEnumAModelLibrary_INTERFACE_DEFINED__

/* interface IEnumAModelLibrary */
/* [object][restricted][oleautomation][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IEnumAModelLibrary;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("8AD467D8-0C32-11D2-9B42-0000C07EE8F2")
    IEnumAModelLibrary : public IUnknown
    {
    public:
        virtual HRESULT __stdcall Next( 
            /* [in] */ unsigned long uCount,
            /* [out] */ IUnknown **ppModel,
            /* [out] */ unsigned long *pnumFetched) = 0;
        
        virtual HRESULT __stdcall Skip( 
            /* [in] */ unsigned long uCount) = 0;
        
        virtual HRESULT __stdcall Reset( void) = 0;
        
        virtual HRESULT __stdcall Clone( 
            /* [out] */ IEnumAModelLibrary **ppEnum) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IEnumAModelLibraryVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IEnumAModelLibrary * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IEnumAModelLibrary * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IEnumAModelLibrary * This);
        
        HRESULT ( __stdcall *Next )( 
            IEnumAModelLibrary * This,
            /* [in] */ unsigned long uCount,
            /* [out] */ IUnknown **ppModel,
            /* [out] */ unsigned long *pnumFetched);
        
        HRESULT ( __stdcall *Skip )( 
            IEnumAModelLibrary * This,
            /* [in] */ unsigned long uCount);
        
        HRESULT ( __stdcall *Reset )( 
            IEnumAModelLibrary * This);
        
        HRESULT ( __stdcall *Clone )( 
            IEnumAModelLibrary * This,
            /* [out] */ IEnumAModelLibrary **ppEnum);
        
        END_INTERFACE
    } IEnumAModelLibraryVtbl;

    interface IEnumAModelLibrary
    {
        CONST_VTBL struct IEnumAModelLibraryVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IEnumAModelLibrary_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumAModelLibrary_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IEnumAModelLibrary_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IEnumAModelLibrary_Next(This,uCount,ppModel,pnumFetched)	\
    (This)->lpVtbl -> Next(This,uCount,ppModel,pnumFetched)

#define IEnumAModelLibrary_Skip(This,uCount)	\
    (This)->lpVtbl -> Skip(This,uCount)

#define IEnumAModelLibrary_Reset(This)	\
    (This)->lpVtbl -> Reset(This)

#define IEnumAModelLibrary_Clone(This,ppEnum)	\
    (This)->lpVtbl -> Clone(This,ppEnum)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT __stdcall IEnumAModelLibrary_Next_Proxy( 
    IEnumAModelLibrary * This,
    /* [in] */ unsigned long uCount,
    /* [out] */ IUnknown **ppModel,
    /* [out] */ unsigned long *pnumFetched);


void __RPC_STUB IEnumAModelLibrary_Next_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT __stdcall IEnumAModelLibrary_Skip_Proxy( 
    IEnumAModelLibrary * This,
    /* [in] */ unsigned long uCount);


void __RPC_STUB IEnumAModelLibrary_Skip_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT __stdcall IEnumAModelLibrary_Reset_Proxy( 
    IEnumAModelLibrary * This);


void __RPC_STUB IEnumAModelLibrary_Reset_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT __stdcall IEnumAModelLibrary_Clone_Proxy( 
    IEnumAModelLibrary * This,
    /* [out] */ IEnumAModelLibrary **ppEnum);


void __RPC_STUB IEnumAModelLibrary_Clone_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IEnumAModelLibrary_INTERFACE_DEFINED__ */


#ifndef __IHNodeCol_INTERFACE_DEFINED__
#define __IHNodeCol_INTERFACE_DEFINED__

/* interface IHNodeCol */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHNodeCol;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("89E81F60-019A-11D0-9BE7-0000C05375D5")
    IHNodeCol : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Application( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parent( 
            /* [retval][out] */ IHapp **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *len) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RowCount( 
            /* [in] */ long Dimension,
            /* [retval][out] */ long *len) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Dimension( 
            /* [retval][out] */ long *Dimension) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsNamedDimension( 
            /* [optional][in] */ VARIANT Dimension,
            /* [retval][out] */ VARIANT_BOOL *is_named) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Item( 
            /* [in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ItemName( 
            /* [in] */ long location,
            /* [optional][in] */ VARIANT Dimension,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *item_name) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ItemName( 
            /* [in] */ long location,
            /* [optional][in] */ VARIANT Dimension,
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR item_name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Label( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *Label) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Label( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR Label) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LabelNode( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][out] */ VARIANT *Label,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LabelLocation( 
            /* [in] */ BSTR Label,
            /* [in] */ long Dimension,
            /* [retval][out] */ long *location) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LabelAttribute( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *retval) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LabelAttribute( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT retval) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LabelAttributeType( 
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [retval][out] */ short *type) = 0;
        
        virtual /* [restricted][propget][id] */ HRESULT STDMETHODCALLTYPE get__NewEnum( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Add( 
            /* [optional][in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Insert( 
            /* [in] */ IHNode *element,
            /* [optional][in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Remove( 
            /* [in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InsertRow( 
            /* [in] */ long Dimension,
            /* [in] */ long location) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveRow( 
            /* [in] */ long Dimension,
            /* [in] */ long location) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DimensionName( 
            /* [in] */ long Dimension,
            /* [retval][out] */ BSTR *dim_name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reorder( 
            /* [in] */ VARIANT loc_or_name,
            /* [in] */ short dir) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHNodeColVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHNodeCol * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHNodeCol * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHNodeCol * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHNodeCol * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHNodeCol * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHNodeCol * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHNodeCol * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Application )( 
            IHNodeCol * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parent )( 
            IHNodeCol * This,
            /* [retval][out] */ IHapp **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IHNodeCol * This,
            /* [retval][out] */ long *len);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RowCount )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [retval][out] */ long *len);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Dimension )( 
            IHNodeCol * This,
            /* [retval][out] */ long *Dimension);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsNamedDimension )( 
            IHNodeCol * This,
            /* [optional][in] */ VARIANT Dimension,
            /* [retval][out] */ VARIANT_BOOL *is_named);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Item )( 
            IHNodeCol * This,
            /* [in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ItemName )( 
            IHNodeCol * This,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT Dimension,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *item_name);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ItemName )( 
            IHNodeCol * This,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT Dimension,
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR item_name);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Label )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ BSTR *Label);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Label )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][in] */ VARIANT force,
            /* [in] */ BSTR Label);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LabelNode )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [optional][out] */ VARIANT *Label,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LabelLocation )( 
            IHNodeCol * This,
            /* [in] */ BSTR Label,
            /* [in] */ long Dimension,
            /* [retval][out] */ long *location);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LabelAttribute )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [optional][in] */ VARIANT force,
            /* [retval][out] */ VARIANT *retval);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LabelAttribute )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [optional][in] */ VARIANT force,
            /* [in] */ VARIANT retval);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LabelAttributeType )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location,
            /* [in] */ short attrnum,
            /* [retval][out] */ short *type);
        
        /* [restricted][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get__NewEnum )( 
            IHNodeCol * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Add )( 
            IHNodeCol * This,
            /* [optional][in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Insert )( 
            IHNodeCol * This,
            /* [in] */ IHNode *element,
            /* [optional][in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Remove )( 
            IHNodeCol * This,
            /* [in] */ VARIANT loc_or_name,
            /* [optional][in] */ VARIANT loc_or_name2,
            /* [optional][in] */ VARIANT loc_or_name3,
            /* [optional][in] */ VARIANT loc_or_name4,
            /* [optional][in] */ VARIANT loc_or_name5,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InsertRow )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveRow )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [in] */ long location);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DimensionName )( 
            IHNodeCol * This,
            /* [in] */ long Dimension,
            /* [retval][out] */ BSTR *dim_name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reorder )( 
            IHNodeCol * This,
            /* [in] */ VARIANT loc_or_name,
            /* [in] */ short dir);
        
        END_INTERFACE
    } IHNodeColVtbl;

    interface IHNodeCol
    {
        CONST_VTBL struct IHNodeColVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHNodeCol_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHNodeCol_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHNodeCol_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHNodeCol_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHNodeCol_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHNodeCol_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHNodeCol_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHNodeCol_get_Application(This,retval)	\
    (This)->lpVtbl -> get_Application(This,retval)

#define IHNodeCol_get_Parent(This,retval)	\
    (This)->lpVtbl -> get_Parent(This,retval)

#define IHNodeCol_get_Count(This,len)	\
    (This)->lpVtbl -> get_Count(This,len)

#define IHNodeCol_get_RowCount(This,Dimension,len)	\
    (This)->lpVtbl -> get_RowCount(This,Dimension,len)

#define IHNodeCol_get_Dimension(This,Dimension)	\
    (This)->lpVtbl -> get_Dimension(This,Dimension)

#define IHNodeCol_get_IsNamedDimension(This,Dimension,is_named)	\
    (This)->lpVtbl -> get_IsNamedDimension(This,Dimension,is_named)

#define IHNodeCol_get_Item(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)	\
    (This)->lpVtbl -> get_Item(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)

#define IHNodeCol_get_ItemName(This,location,Dimension,force,item_name)	\
    (This)->lpVtbl -> get_ItemName(This,location,Dimension,force,item_name)

#define IHNodeCol_put_ItemName(This,location,Dimension,force,item_name)	\
    (This)->lpVtbl -> put_ItemName(This,location,Dimension,force,item_name)

#define IHNodeCol_get_Label(This,Dimension,location,force,Label)	\
    (This)->lpVtbl -> get_Label(This,Dimension,location,force,Label)

#define IHNodeCol_put_Label(This,Dimension,location,force,Label)	\
    (This)->lpVtbl -> put_Label(This,Dimension,location,force,Label)

#define IHNodeCol_get_LabelNode(This,Dimension,location,Label,retval)	\
    (This)->lpVtbl -> get_LabelNode(This,Dimension,location,Label,retval)

#define IHNodeCol_get_LabelLocation(This,Label,Dimension,location)	\
    (This)->lpVtbl -> get_LabelLocation(This,Label,Dimension,location)

#define IHNodeCol_get_LabelAttribute(This,Dimension,location,attrnum,force,retval)	\
    (This)->lpVtbl -> get_LabelAttribute(This,Dimension,location,attrnum,force,retval)

#define IHNodeCol_put_LabelAttribute(This,Dimension,location,attrnum,force,retval)	\
    (This)->lpVtbl -> put_LabelAttribute(This,Dimension,location,attrnum,force,retval)

#define IHNodeCol_get_LabelAttributeType(This,Dimension,location,attrnum,type)	\
    (This)->lpVtbl -> get_LabelAttributeType(This,Dimension,location,attrnum,type)

#define IHNodeCol_get__NewEnum(This,retval)	\
    (This)->lpVtbl -> get__NewEnum(This,retval)

#define IHNodeCol_Add(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)	\
    (This)->lpVtbl -> Add(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)

#define IHNodeCol_Insert(This,element,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5)	\
    (This)->lpVtbl -> Insert(This,element,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5)

#define IHNodeCol_Remove(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)	\
    (This)->lpVtbl -> Remove(This,loc_or_name,loc_or_name2,loc_or_name3,loc_or_name4,loc_or_name5,retval)

#define IHNodeCol_InsertRow(This,Dimension,location)	\
    (This)->lpVtbl -> InsertRow(This,Dimension,location)

#define IHNodeCol_RemoveRow(This,Dimension,location)	\
    (This)->lpVtbl -> RemoveRow(This,Dimension,location)

#define IHNodeCol_get_DimensionName(This,Dimension,dim_name)	\
    (This)->lpVtbl -> get_DimensionName(This,Dimension,dim_name)

#define IHNodeCol_Reorder(This,loc_or_name,dir)	\
    (This)->lpVtbl -> Reorder(This,loc_or_name,dir)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Application_Proxy( 
    IHNodeCol * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHNodeCol_get_Application_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Parent_Proxy( 
    IHNodeCol * This,
    /* [retval][out] */ IHapp **retval);


void __RPC_STUB IHNodeCol_get_Parent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Count_Proxy( 
    IHNodeCol * This,
    /* [retval][out] */ long *len);


void __RPC_STUB IHNodeCol_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_RowCount_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [retval][out] */ long *len);


void __RPC_STUB IHNodeCol_get_RowCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Dimension_Proxy( 
    IHNodeCol * This,
    /* [retval][out] */ long *Dimension);


void __RPC_STUB IHNodeCol_get_Dimension_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_IsNamedDimension_Proxy( 
    IHNodeCol * This,
    /* [optional][in] */ VARIANT Dimension,
    /* [retval][out] */ VARIANT_BOOL *is_named);


void __RPC_STUB IHNodeCol_get_IsNamedDimension_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Item_Proxy( 
    IHNodeCol * This,
    /* [in] */ VARIANT loc_or_name,
    /* [optional][in] */ VARIANT loc_or_name2,
    /* [optional][in] */ VARIANT loc_or_name3,
    /* [optional][in] */ VARIANT loc_or_name4,
    /* [optional][in] */ VARIANT loc_or_name5,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHNodeCol_get_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_ItemName_Proxy( 
    IHNodeCol * This,
    /* [in] */ long location,
    /* [optional][in] */ VARIANT Dimension,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ BSTR *item_name);


void __RPC_STUB IHNodeCol_get_ItemName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_put_ItemName_Proxy( 
    IHNodeCol * This,
    /* [in] */ long location,
    /* [optional][in] */ VARIANT Dimension,
    /* [optional][in] */ VARIANT force,
    /* [in] */ BSTR item_name);


void __RPC_STUB IHNodeCol_put_ItemName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_Label_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ BSTR *Label);


void __RPC_STUB IHNodeCol_get_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_put_Label_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [optional][in] */ VARIANT force,
    /* [in] */ BSTR Label);


void __RPC_STUB IHNodeCol_put_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_LabelNode_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [optional][out] */ VARIANT *Label,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHNodeCol_get_LabelNode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_LabelLocation_Proxy( 
    IHNodeCol * This,
    /* [in] */ BSTR Label,
    /* [in] */ long Dimension,
    /* [retval][out] */ long *location);


void __RPC_STUB IHNodeCol_get_LabelLocation_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_LabelAttribute_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [in] */ short attrnum,
    /* [optional][in] */ VARIANT force,
    /* [retval][out] */ VARIANT *retval);


void __RPC_STUB IHNodeCol_get_LabelAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_put_LabelAttribute_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [in] */ short attrnum,
    /* [optional][in] */ VARIANT force,
    /* [in] */ VARIANT retval);


void __RPC_STUB IHNodeCol_put_LabelAttribute_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_LabelAttributeType_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location,
    /* [in] */ short attrnum,
    /* [retval][out] */ short *type);


void __RPC_STUB IHNodeCol_get_LabelAttributeType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [restricted][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get__NewEnum_Proxy( 
    IHNodeCol * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IHNodeCol_get__NewEnum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_Add_Proxy( 
    IHNodeCol * This,
    /* [optional][in] */ VARIANT loc_or_name,
    /* [optional][in] */ VARIANT loc_or_name2,
    /* [optional][in] */ VARIANT loc_or_name3,
    /* [optional][in] */ VARIANT loc_or_name4,
    /* [optional][in] */ VARIANT loc_or_name5,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHNodeCol_Add_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_Insert_Proxy( 
    IHNodeCol * This,
    /* [in] */ IHNode *element,
    /* [optional][in] */ VARIANT loc_or_name,
    /* [optional][in] */ VARIANT loc_or_name2,
    /* [optional][in] */ VARIANT loc_or_name3,
    /* [optional][in] */ VARIANT loc_or_name4,
    /* [optional][in] */ VARIANT loc_or_name5);


void __RPC_STUB IHNodeCol_Insert_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_Remove_Proxy( 
    IHNodeCol * This,
    /* [in] */ VARIANT loc_or_name,
    /* [optional][in] */ VARIANT loc_or_name2,
    /* [optional][in] */ VARIANT loc_or_name3,
    /* [optional][in] */ VARIANT loc_or_name4,
    /* [optional][in] */ VARIANT loc_or_name5,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHNodeCol_Remove_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_InsertRow_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location);


void __RPC_STUB IHNodeCol_InsertRow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_RemoveRow_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [in] */ long location);


void __RPC_STUB IHNodeCol_RemoveRow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_get_DimensionName_Proxy( 
    IHNodeCol * This,
    /* [in] */ long Dimension,
    /* [retval][out] */ BSTR *dim_name);


void __RPC_STUB IHNodeCol_get_DimensionName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeCol_Reorder_Proxy( 
    IHNodeCol * This,
    /* [in] */ VARIANT loc_or_name,
    /* [in] */ short dir);


void __RPC_STUB IHNodeCol_Reorder_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHNodeCol_INTERFACE_DEFINED__ */


#ifndef __IScrollAreaInfo_INTERFACE_DEFINED__
#define __IScrollAreaInfo_INTERFACE_DEFINED__

/* interface IScrollAreaInfo */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IScrollAreaInfo;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("657E4330-ED8D-11D0-A991-0000C0237DF9")
    IScrollAreaInfo : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NodeAt( 
            /* [in] */ long row,
            /* [in] */ long col,
            /* [in] */ short index,
            /* [retval][out] */ IHNode **ppnode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetMaximum( 
            /* [out][in] */ long *pRow,
            /* [out][in] */ short *pRowExt,
            /* [out][in] */ long *pCol,
            /* [out][in] */ short *pColExt) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IScrollAreaInfoVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IScrollAreaInfo * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IScrollAreaInfo * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IScrollAreaInfo * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IScrollAreaInfo * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IScrollAreaInfo * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IScrollAreaInfo * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IScrollAreaInfo * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NodeAt )( 
            IScrollAreaInfo * This,
            /* [in] */ long row,
            /* [in] */ long col,
            /* [in] */ short index,
            /* [retval][out] */ IHNode **ppnode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetMaximum )( 
            IScrollAreaInfo * This,
            /* [out][in] */ long *pRow,
            /* [out][in] */ short *pRowExt,
            /* [out][in] */ long *pCol,
            /* [out][in] */ short *pColExt);
        
        END_INTERFACE
    } IScrollAreaInfoVtbl;

    interface IScrollAreaInfo
    {
        CONST_VTBL struct IScrollAreaInfoVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IScrollAreaInfo_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IScrollAreaInfo_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IScrollAreaInfo_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IScrollAreaInfo_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IScrollAreaInfo_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IScrollAreaInfo_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IScrollAreaInfo_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IScrollAreaInfo_get_NodeAt(This,row,col,index,ppnode)	\
    (This)->lpVtbl -> get_NodeAt(This,row,col,index,ppnode)

#define IScrollAreaInfo_GetMaximum(This,pRow,pRowExt,pCol,pColExt)	\
    (This)->lpVtbl -> GetMaximum(This,pRow,pRowExt,pCol,pColExt)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IScrollAreaInfo_get_NodeAt_Proxy( 
    IScrollAreaInfo * This,
    /* [in] */ long row,
    /* [in] */ long col,
    /* [in] */ short index,
    /* [retval][out] */ IHNode **ppnode);


void __RPC_STUB IScrollAreaInfo_get_NodeAt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IScrollAreaInfo_GetMaximum_Proxy( 
    IScrollAreaInfo * This,
    /* [out][in] */ long *pRow,
    /* [out][in] */ short *pRowExt,
    /* [out][in] */ long *pCol,
    /* [out][in] */ short *pColExt);


void __RPC_STUB IScrollAreaInfo_GetMaximum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IScrollAreaInfo_INTERFACE_DEFINED__ */


#ifndef __IHPlotVal_INTERFACE_DEFINED__
#define __IHPlotVal_INTERFACE_DEFINED__

/* interface IHPlotVal */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHPlotVal;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("11677780-B72B-11D0-A943-0000C0A118DB")
    IHPlotVal : public IDispatch
    {
    public:
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Context( 
            /* [in] */ IHNode *pContext) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Context( 
            /* [retval][out] */ IHNode **pContext) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Title( 
            /* [retval][out] */ BSTR *Title) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TitleInt( 
            /* [retval][out] */ BSTR *Title) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Title( 
            /* [in] */ BSTR Title) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XAxisTitle( 
            /* [retval][out] */ BSTR *xtitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XAxisTitleInt( 
            /* [retval][out] */ BSTR *xtitle) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_XAxisTitle( 
            /* [in] */ BSTR xtitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YAxisTitle( 
            /* [optional][in] */ VARIANT nAxis,
            /* [retval][out] */ BSTR *ytitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YAxisTitleInt( 
            /* [optional][in] */ VARIANT nAxis,
            /* [retval][out] */ BSTR *ytitle) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_YAxisTitle( 
            /* [optional][in] */ VARIANT nAxis,
            /* [in] */ BSTR ytitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Label( 
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Label) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LabelInt( 
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Label) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Label( 
            /* [in] */ long index,
            /* [in] */ BSTR Label) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NPoints( 
            /* [retval][out] */ long *NPoints) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NPoints( 
            /* [in] */ long NPoints) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NVariables( 
            /* [retval][out] */ long *nvar) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NVariables( 
            /* [in] */ long nvar) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NXVariables( 
            /* [retval][out] */ long *nvar) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NXVariables( 
            /* [in] */ long nvar) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XDataValue( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_XData( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [in] */ short attr,
            /* [in] */ VARIANT rhs) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YDataValue( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_YData( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [in] */ short attr,
            /* [in] */ VARIANT rhs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetXData( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [out] */ short *attr,
            /* [out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetYData( 
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [out] */ short *attr,
            /* [out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwapAxises( 
            /* [retval][out] */ VARIANT_BOOL *pbSwap) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwapAxises( 
            /* [in] */ VARIANT_BOOL pbSwap) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseXAxis( 
            /* [retval][out] */ VARIANT_BOOL *pbReverse) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseXAxis( 
            /* [in] */ VARIANT_BOOL pbReverse) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AddTimeStamp( 
            /* [retval][out] */ VARIANT_BOOL *pbTimeStamp) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AddTimeStamp( 
            /* [in] */ VARIANT_BOOL pbTimeStamp) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ShowLegend( 
            /* [retval][out] */ VARIANT_BOOL *pbLegend) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ShowLegend( 
            /* [in] */ VARIANT_BOOL pbLegend) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PlotType( 
            /* [retval][out] */ long *pPlotType) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PlotType( 
            /* [in] */ long pPlotType) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parametric( 
            /* [retval][out] */ VARIANT_BOOL *pbParametric) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Parametric( 
            /* [in] */ VARIANT_BOOL pbParametric) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZAxisTitle( 
            /* [retval][out] */ BSTR *ztitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZAxisTitleInt( 
            /* [retval][out] */ BSTR *ztitle) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ZAxisTitle( 
            /* [in] */ BSTR ztitle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Live( 
            /* [retval][out] */ VARIANT_BOOL *pbLive) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Live( 
            /* [in] */ VARIANT_BOOL pbLive) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddAnnotationText( 
            /* [in] */ PLOTTEXT *text) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AnnotationCount( 
            /* [retval][out] */ long *Count) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AnnotationText( 
            /* [in] */ long index,
            /* [retval][out] */ PLOTTEXT *text) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GridType( 
            /* [retval][out] */ short *grid) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GridType( 
            /* [in] */ short grid) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MarkerSize( 
            /* [retval][out] */ short *marker) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MarkerSize( 
            /* [in] */ short marker) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ShowDiagonalLine( 
            /* [retval][out] */ VARIANT_BOOL *pbDiagonal) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ShowDiagonalLine( 
            /* [in] */ VARIANT_BOOL pbDiagonal) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LineStyle( 
            /* [retval][out] */ short *line) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LineStyle( 
            /* [in] */ short line) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XDataInteger( 
            /* [in] */ long index,
            /* [retval][out] */ VARIANT_BOOL *pbInteger) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ShowZeroLine( 
            /* [retval][out] */ VARIANT_BOOL *pbZero) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ShowZeroLine( 
            /* [in] */ VARIANT_BOOL pbZero) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AxisMap( 
            /* [in] */ long index,
            /* [retval][out] */ long *pAxis) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AxisMap( 
            /* [in] */ long index,
            /* [in] */ long pAxis) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CurveStyle( 
            /* [in] */ long index,
            /* [retval][out] */ long *pStyle) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CurveStyle( 
            /* [in] */ long index,
            /* [in] */ long pStyle) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveAnnotationText( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SquarePlot( 
            /* [retval][out] */ VARIANT_BOOL *pbSquare) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SquarePlot( 
            /* [in] */ VARIANT_BOOL pbSquare) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CanLive( 
            /* [retval][out] */ VARIANT_BOOL *pbCanLive) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CanLive( 
            /* [in] */ VARIANT_BOOL pbCanLive) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AxisScale( 
            /* [in] */ long VarNo,
            /* [retval][out] */ long *pScale) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AxisScale( 
            /* [in] */ long VarNo,
            /* [in] */ long pScale) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHPlotValVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHPlotVal * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHPlotVal * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHPlotVal * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHPlotVal * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHPlotVal * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHPlotVal * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHPlotVal * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Context )( 
            IHPlotVal * This,
            /* [in] */ IHNode *pContext);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Context )( 
            IHPlotVal * This,
            /* [retval][out] */ IHNode **pContext);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Title )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *Title);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TitleInt )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *Title);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Title )( 
            IHPlotVal * This,
            /* [in] */ BSTR Title);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XAxisTitle )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *xtitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XAxisTitleInt )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *xtitle);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_XAxisTitle )( 
            IHPlotVal * This,
            /* [in] */ BSTR xtitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YAxisTitle )( 
            IHPlotVal * This,
            /* [optional][in] */ VARIANT nAxis,
            /* [retval][out] */ BSTR *ytitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YAxisTitleInt )( 
            IHPlotVal * This,
            /* [optional][in] */ VARIANT nAxis,
            /* [retval][out] */ BSTR *ytitle);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_YAxisTitle )( 
            IHPlotVal * This,
            /* [optional][in] */ VARIANT nAxis,
            /* [in] */ BSTR ytitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Label )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Label);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LabelInt )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Label);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Label )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ BSTR Label);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NPoints )( 
            IHPlotVal * This,
            /* [retval][out] */ long *NPoints);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NPoints )( 
            IHPlotVal * This,
            /* [in] */ long NPoints);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NVariables )( 
            IHPlotVal * This,
            /* [retval][out] */ long *nvar);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NVariables )( 
            IHPlotVal * This,
            /* [in] */ long nvar);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NXVariables )( 
            IHPlotVal * This,
            /* [retval][out] */ long *nvar);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NXVariables )( 
            IHPlotVal * This,
            /* [in] */ long nvar);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XDataValue )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [retval][out] */ double *Value);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_XData )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [in] */ short attr,
            /* [in] */ VARIANT rhs);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YDataValue )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [retval][out] */ double *Value);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_YData )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [in] */ short attr,
            /* [in] */ VARIANT rhs);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetXData )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [out] */ short *attr,
            /* [out] */ VARIANT *Value);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetYData )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long datano,
            /* [out] */ short *attr,
            /* [out] */ VARIANT *Value);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IHPlotVal * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwapAxises )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbSwap);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwapAxises )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbSwap);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseXAxis )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbReverse);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseXAxis )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbReverse);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AddTimeStamp )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbTimeStamp);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AddTimeStamp )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbTimeStamp);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ShowLegend )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbLegend);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ShowLegend )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbLegend);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PlotType )( 
            IHPlotVal * This,
            /* [retval][out] */ long *pPlotType);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PlotType )( 
            IHPlotVal * This,
            /* [in] */ long pPlotType);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parametric )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbParametric);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Parametric )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbParametric);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZAxisTitle )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *ztitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZAxisTitleInt )( 
            IHPlotVal * This,
            /* [retval][out] */ BSTR *ztitle);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ZAxisTitle )( 
            IHPlotVal * This,
            /* [in] */ BSTR ztitle);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Live )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbLive);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Live )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddAnnotationText )( 
            IHPlotVal * This,
            /* [in] */ PLOTTEXT *text);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AnnotationCount )( 
            IHPlotVal * This,
            /* [retval][out] */ long *Count);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AnnotationText )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ PLOTTEXT *text);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GridType )( 
            IHPlotVal * This,
            /* [retval][out] */ short *grid);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GridType )( 
            IHPlotVal * This,
            /* [in] */ short grid);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MarkerSize )( 
            IHPlotVal * This,
            /* [retval][out] */ short *marker);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MarkerSize )( 
            IHPlotVal * This,
            /* [in] */ short marker);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ShowDiagonalLine )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbDiagonal);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ShowDiagonalLine )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbDiagonal);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LineStyle )( 
            IHPlotVal * This,
            /* [retval][out] */ short *line);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LineStyle )( 
            IHPlotVal * This,
            /* [in] */ short line);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XDataInteger )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ VARIANT_BOOL *pbInteger);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ShowZeroLine )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbZero);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ShowZeroLine )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbZero);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AxisMap )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ long *pAxis);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AxisMap )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long pAxis);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CurveStyle )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [retval][out] */ long *pStyle);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CurveStyle )( 
            IHPlotVal * This,
            /* [in] */ long index,
            /* [in] */ long pStyle);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveAnnotationText )( 
            IHPlotVal * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SquarePlot )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbSquare);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SquarePlot )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbSquare);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CanLive )( 
            IHPlotVal * This,
            /* [retval][out] */ VARIANT_BOOL *pbCanLive);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CanLive )( 
            IHPlotVal * This,
            /* [in] */ VARIANT_BOOL pbCanLive);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AxisScale )( 
            IHPlotVal * This,
            /* [in] */ long VarNo,
            /* [retval][out] */ long *pScale);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AxisScale )( 
            IHPlotVal * This,
            /* [in] */ long VarNo,
            /* [in] */ long pScale);
        
        END_INTERFACE
    } IHPlotValVtbl;

    interface IHPlotVal
    {
        CONST_VTBL struct IHPlotValVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHPlotVal_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHPlotVal_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHPlotVal_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHPlotVal_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHPlotVal_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHPlotVal_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHPlotVal_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHPlotVal_put_Context(This,pContext)	\
    (This)->lpVtbl -> put_Context(This,pContext)

#define IHPlotVal_get_Context(This,pContext)	\
    (This)->lpVtbl -> get_Context(This,pContext)

#define IHPlotVal_get_Title(This,Title)	\
    (This)->lpVtbl -> get_Title(This,Title)

#define IHPlotVal_get_TitleInt(This,Title)	\
    (This)->lpVtbl -> get_TitleInt(This,Title)

#define IHPlotVal_put_Title(This,Title)	\
    (This)->lpVtbl -> put_Title(This,Title)

#define IHPlotVal_get_XAxisTitle(This,xtitle)	\
    (This)->lpVtbl -> get_XAxisTitle(This,xtitle)

#define IHPlotVal_get_XAxisTitleInt(This,xtitle)	\
    (This)->lpVtbl -> get_XAxisTitleInt(This,xtitle)

#define IHPlotVal_put_XAxisTitle(This,xtitle)	\
    (This)->lpVtbl -> put_XAxisTitle(This,xtitle)

#define IHPlotVal_get_YAxisTitle(This,nAxis,ytitle)	\
    (This)->lpVtbl -> get_YAxisTitle(This,nAxis,ytitle)

#define IHPlotVal_get_YAxisTitleInt(This,nAxis,ytitle)	\
    (This)->lpVtbl -> get_YAxisTitleInt(This,nAxis,ytitle)

#define IHPlotVal_put_YAxisTitle(This,nAxis,ytitle)	\
    (This)->lpVtbl -> put_YAxisTitle(This,nAxis,ytitle)

#define IHPlotVal_get_Label(This,index,Label)	\
    (This)->lpVtbl -> get_Label(This,index,Label)

#define IHPlotVal_get_LabelInt(This,index,Label)	\
    (This)->lpVtbl -> get_LabelInt(This,index,Label)

#define IHPlotVal_put_Label(This,index,Label)	\
    (This)->lpVtbl -> put_Label(This,index,Label)

#define IHPlotVal_get_NPoints(This,NPoints)	\
    (This)->lpVtbl -> get_NPoints(This,NPoints)

#define IHPlotVal_put_NPoints(This,NPoints)	\
    (This)->lpVtbl -> put_NPoints(This,NPoints)

#define IHPlotVal_get_NVariables(This,nvar)	\
    (This)->lpVtbl -> get_NVariables(This,nvar)

#define IHPlotVal_put_NVariables(This,nvar)	\
    (This)->lpVtbl -> put_NVariables(This,nvar)

#define IHPlotVal_get_NXVariables(This,nvar)	\
    (This)->lpVtbl -> get_NXVariables(This,nvar)

#define IHPlotVal_put_NXVariables(This,nvar)	\
    (This)->lpVtbl -> put_NXVariables(This,nvar)

#define IHPlotVal_get_XDataValue(This,index,datano,Value)	\
    (This)->lpVtbl -> get_XDataValue(This,index,datano,Value)

#define IHPlotVal_put_XData(This,index,datano,attr,rhs)	\
    (This)->lpVtbl -> put_XData(This,index,datano,attr,rhs)

#define IHPlotVal_get_YDataValue(This,index,datano,Value)	\
    (This)->lpVtbl -> get_YDataValue(This,index,datano,Value)

#define IHPlotVal_put_YData(This,index,datano,attr,rhs)	\
    (This)->lpVtbl -> put_YData(This,index,datano,attr,rhs)

#define IHPlotVal_GetXData(This,index,datano,attr,Value)	\
    (This)->lpVtbl -> GetXData(This,index,datano,attr,Value)

#define IHPlotVal_GetYData(This,index,datano,attr,Value)	\
    (This)->lpVtbl -> GetYData(This,index,datano,attr,Value)

#define IHPlotVal_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IHPlotVal_get_SwapAxises(This,pbSwap)	\
    (This)->lpVtbl -> get_SwapAxises(This,pbSwap)

#define IHPlotVal_put_SwapAxises(This,pbSwap)	\
    (This)->lpVtbl -> put_SwapAxises(This,pbSwap)

#define IHPlotVal_get_ReverseXAxis(This,pbReverse)	\
    (This)->lpVtbl -> get_ReverseXAxis(This,pbReverse)

#define IHPlotVal_put_ReverseXAxis(This,pbReverse)	\
    (This)->lpVtbl -> put_ReverseXAxis(This,pbReverse)

#define IHPlotVal_get_AddTimeStamp(This,pbTimeStamp)	\
    (This)->lpVtbl -> get_AddTimeStamp(This,pbTimeStamp)

#define IHPlotVal_put_AddTimeStamp(This,pbTimeStamp)	\
    (This)->lpVtbl -> put_AddTimeStamp(This,pbTimeStamp)

#define IHPlotVal_get_ShowLegend(This,pbLegend)	\
    (This)->lpVtbl -> get_ShowLegend(This,pbLegend)

#define IHPlotVal_put_ShowLegend(This,pbLegend)	\
    (This)->lpVtbl -> put_ShowLegend(This,pbLegend)

#define IHPlotVal_get_PlotType(This,pPlotType)	\
    (This)->lpVtbl -> get_PlotType(This,pPlotType)

#define IHPlotVal_put_PlotType(This,pPlotType)	\
    (This)->lpVtbl -> put_PlotType(This,pPlotType)

#define IHPlotVal_get_Parametric(This,pbParametric)	\
    (This)->lpVtbl -> get_Parametric(This,pbParametric)

#define IHPlotVal_put_Parametric(This,pbParametric)	\
    (This)->lpVtbl -> put_Parametric(This,pbParametric)

#define IHPlotVal_get_ZAxisTitle(This,ztitle)	\
    (This)->lpVtbl -> get_ZAxisTitle(This,ztitle)

#define IHPlotVal_get_ZAxisTitleInt(This,ztitle)	\
    (This)->lpVtbl -> get_ZAxisTitleInt(This,ztitle)

#define IHPlotVal_put_ZAxisTitle(This,ztitle)	\
    (This)->lpVtbl -> put_ZAxisTitle(This,ztitle)

#define IHPlotVal_get_Live(This,pbLive)	\
    (This)->lpVtbl -> get_Live(This,pbLive)

#define IHPlotVal_put_Live(This,pbLive)	\
    (This)->lpVtbl -> put_Live(This,pbLive)

#define IHPlotVal_AddAnnotationText(This,text)	\
    (This)->lpVtbl -> AddAnnotationText(This,text)

#define IHPlotVal_get_AnnotationCount(This,Count)	\
    (This)->lpVtbl -> get_AnnotationCount(This,Count)

#define IHPlotVal_get_AnnotationText(This,index,text)	\
    (This)->lpVtbl -> get_AnnotationText(This,index,text)

#define IHPlotVal_get_GridType(This,grid)	\
    (This)->lpVtbl -> get_GridType(This,grid)

#define IHPlotVal_put_GridType(This,grid)	\
    (This)->lpVtbl -> put_GridType(This,grid)

#define IHPlotVal_get_MarkerSize(This,marker)	\
    (This)->lpVtbl -> get_MarkerSize(This,marker)

#define IHPlotVal_put_MarkerSize(This,marker)	\
    (This)->lpVtbl -> put_MarkerSize(This,marker)

#define IHPlotVal_get_ShowDiagonalLine(This,pbDiagonal)	\
    (This)->lpVtbl -> get_ShowDiagonalLine(This,pbDiagonal)

#define IHPlotVal_put_ShowDiagonalLine(This,pbDiagonal)	\
    (This)->lpVtbl -> put_ShowDiagonalLine(This,pbDiagonal)

#define IHPlotVal_get_LineStyle(This,line)	\
    (This)->lpVtbl -> get_LineStyle(This,line)

#define IHPlotVal_put_LineStyle(This,line)	\
    (This)->lpVtbl -> put_LineStyle(This,line)

#define IHPlotVal_get_XDataInteger(This,index,pbInteger)	\
    (This)->lpVtbl -> get_XDataInteger(This,index,pbInteger)

#define IHPlotVal_get_ShowZeroLine(This,pbZero)	\
    (This)->lpVtbl -> get_ShowZeroLine(This,pbZero)

#define IHPlotVal_put_ShowZeroLine(This,pbZero)	\
    (This)->lpVtbl -> put_ShowZeroLine(This,pbZero)

#define IHPlotVal_get_AxisMap(This,index,pAxis)	\
    (This)->lpVtbl -> get_AxisMap(This,index,pAxis)

#define IHPlotVal_put_AxisMap(This,index,pAxis)	\
    (This)->lpVtbl -> put_AxisMap(This,index,pAxis)

#define IHPlotVal_get_CurveStyle(This,index,pStyle)	\
    (This)->lpVtbl -> get_CurveStyle(This,index,pStyle)

#define IHPlotVal_put_CurveStyle(This,index,pStyle)	\
    (This)->lpVtbl -> put_CurveStyle(This,index,pStyle)

#define IHPlotVal_RemoveAnnotationText(This)	\
    (This)->lpVtbl -> RemoveAnnotationText(This)

#define IHPlotVal_get_SquarePlot(This,pbSquare)	\
    (This)->lpVtbl -> get_SquarePlot(This,pbSquare)

#define IHPlotVal_put_SquarePlot(This,pbSquare)	\
    (This)->lpVtbl -> put_SquarePlot(This,pbSquare)

#define IHPlotVal_get_CanLive(This,pbCanLive)	\
    (This)->lpVtbl -> get_CanLive(This,pbCanLive)

#define IHPlotVal_put_CanLive(This,pbCanLive)	\
    (This)->lpVtbl -> put_CanLive(This,pbCanLive)

#define IHPlotVal_get_AxisScale(This,VarNo,pScale)	\
    (This)->lpVtbl -> get_AxisScale(This,VarNo,pScale)

#define IHPlotVal_put_AxisScale(This,VarNo,pScale)	\
    (This)->lpVtbl -> put_AxisScale(This,VarNo,pScale)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_Context_Proxy( 
    IHPlotVal * This,
    /* [in] */ IHNode *pContext);


void __RPC_STUB IHPlotVal_put_Context_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_Context_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ IHNode **pContext);


void __RPC_STUB IHPlotVal_get_Context_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_Title_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *Title);


void __RPC_STUB IHPlotVal_get_Title_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_TitleInt_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *Title);


void __RPC_STUB IHPlotVal_get_TitleInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_Title_Proxy( 
    IHPlotVal * This,
    /* [in] */ BSTR Title);


void __RPC_STUB IHPlotVal_put_Title_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_XAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *xtitle);


void __RPC_STUB IHPlotVal_get_XAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_XAxisTitleInt_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *xtitle);


void __RPC_STUB IHPlotVal_get_XAxisTitleInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_XAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [in] */ BSTR xtitle);


void __RPC_STUB IHPlotVal_put_XAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_YAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [optional][in] */ VARIANT nAxis,
    /* [retval][out] */ BSTR *ytitle);


void __RPC_STUB IHPlotVal_get_YAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_YAxisTitleInt_Proxy( 
    IHPlotVal * This,
    /* [optional][in] */ VARIANT nAxis,
    /* [retval][out] */ BSTR *ytitle);


void __RPC_STUB IHPlotVal_get_YAxisTitleInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_YAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [optional][in] */ VARIANT nAxis,
    /* [in] */ BSTR ytitle);


void __RPC_STUB IHPlotVal_put_YAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_Label_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ BSTR *Label);


void __RPC_STUB IHPlotVal_get_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_LabelInt_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ BSTR *Label);


void __RPC_STUB IHPlotVal_get_LabelInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_Label_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ BSTR Label);


void __RPC_STUB IHPlotVal_put_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_NPoints_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ long *NPoints);


void __RPC_STUB IHPlotVal_get_NPoints_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_NPoints_Proxy( 
    IHPlotVal * This,
    /* [in] */ long NPoints);


void __RPC_STUB IHPlotVal_put_NPoints_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_NVariables_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ long *nvar);


void __RPC_STUB IHPlotVal_get_NVariables_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_NVariables_Proxy( 
    IHPlotVal * This,
    /* [in] */ long nvar);


void __RPC_STUB IHPlotVal_put_NVariables_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_NXVariables_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ long *nvar);


void __RPC_STUB IHPlotVal_get_NXVariables_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_NXVariables_Proxy( 
    IHPlotVal * This,
    /* [in] */ long nvar);


void __RPC_STUB IHPlotVal_put_NXVariables_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_XDataValue_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [retval][out] */ double *Value);


void __RPC_STUB IHPlotVal_get_XDataValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_XData_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [in] */ short attr,
    /* [in] */ VARIANT rhs);


void __RPC_STUB IHPlotVal_put_XData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_YDataValue_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [retval][out] */ double *Value);


void __RPC_STUB IHPlotVal_get_YDataValue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_YData_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [in] */ short attr,
    /* [in] */ VARIANT rhs);


void __RPC_STUB IHPlotVal_put_YData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_GetXData_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [out] */ short *attr,
    /* [out] */ VARIANT *Value);


void __RPC_STUB IHPlotVal_GetXData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_GetYData_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long datano,
    /* [out] */ short *attr,
    /* [out] */ VARIANT *Value);


void __RPC_STUB IHPlotVal_GetYData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_Clear_Proxy( 
    IHPlotVal * This);


void __RPC_STUB IHPlotVal_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_SwapAxises_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbSwap);


void __RPC_STUB IHPlotVal_get_SwapAxises_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_SwapAxises_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbSwap);


void __RPC_STUB IHPlotVal_put_SwapAxises_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ReverseXAxis_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbReverse);


void __RPC_STUB IHPlotVal_get_ReverseXAxis_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_ReverseXAxis_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbReverse);


void __RPC_STUB IHPlotVal_put_ReverseXAxis_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_AddTimeStamp_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbTimeStamp);


void __RPC_STUB IHPlotVal_get_AddTimeStamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_AddTimeStamp_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbTimeStamp);


void __RPC_STUB IHPlotVal_put_AddTimeStamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ShowLegend_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbLegend);


void __RPC_STUB IHPlotVal_get_ShowLegend_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_ShowLegend_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbLegend);


void __RPC_STUB IHPlotVal_put_ShowLegend_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_PlotType_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ long *pPlotType);


void __RPC_STUB IHPlotVal_get_PlotType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_PlotType_Proxy( 
    IHPlotVal * This,
    /* [in] */ long pPlotType);


void __RPC_STUB IHPlotVal_put_PlotType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_Parametric_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbParametric);


void __RPC_STUB IHPlotVal_get_Parametric_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_Parametric_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbParametric);


void __RPC_STUB IHPlotVal_put_Parametric_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ZAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *ztitle);


void __RPC_STUB IHPlotVal_get_ZAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ZAxisTitleInt_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ BSTR *ztitle);


void __RPC_STUB IHPlotVal_get_ZAxisTitleInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_ZAxisTitle_Proxy( 
    IHPlotVal * This,
    /* [in] */ BSTR ztitle);


void __RPC_STUB IHPlotVal_put_ZAxisTitle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_Live_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbLive);


void __RPC_STUB IHPlotVal_get_Live_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_Live_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbLive);


void __RPC_STUB IHPlotVal_put_Live_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_AddAnnotationText_Proxy( 
    IHPlotVal * This,
    /* [in] */ PLOTTEXT *text);


void __RPC_STUB IHPlotVal_AddAnnotationText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_AnnotationCount_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ long *Count);


void __RPC_STUB IHPlotVal_get_AnnotationCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_AnnotationText_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ PLOTTEXT *text);


void __RPC_STUB IHPlotVal_get_AnnotationText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_GridType_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ short *grid);


void __RPC_STUB IHPlotVal_get_GridType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_GridType_Proxy( 
    IHPlotVal * This,
    /* [in] */ short grid);


void __RPC_STUB IHPlotVal_put_GridType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_MarkerSize_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ short *marker);


void __RPC_STUB IHPlotVal_get_MarkerSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_MarkerSize_Proxy( 
    IHPlotVal * This,
    /* [in] */ short marker);


void __RPC_STUB IHPlotVal_put_MarkerSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ShowDiagonalLine_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbDiagonal);


void __RPC_STUB IHPlotVal_get_ShowDiagonalLine_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_ShowDiagonalLine_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbDiagonal);


void __RPC_STUB IHPlotVal_put_ShowDiagonalLine_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_LineStyle_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ short *line);


void __RPC_STUB IHPlotVal_get_LineStyle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_LineStyle_Proxy( 
    IHPlotVal * This,
    /* [in] */ short line);


void __RPC_STUB IHPlotVal_put_LineStyle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_XDataInteger_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ VARIANT_BOOL *pbInteger);


void __RPC_STUB IHPlotVal_get_XDataInteger_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_ShowZeroLine_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbZero);


void __RPC_STUB IHPlotVal_get_ShowZeroLine_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_ShowZeroLine_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbZero);


void __RPC_STUB IHPlotVal_put_ShowZeroLine_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_AxisMap_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ long *pAxis);


void __RPC_STUB IHPlotVal_get_AxisMap_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_AxisMap_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long pAxis);


void __RPC_STUB IHPlotVal_put_AxisMap_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_CurveStyle_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [retval][out] */ long *pStyle);


void __RPC_STUB IHPlotVal_get_CurveStyle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_CurveStyle_Proxy( 
    IHPlotVal * This,
    /* [in] */ long index,
    /* [in] */ long pStyle);


void __RPC_STUB IHPlotVal_put_CurveStyle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_RemoveAnnotationText_Proxy( 
    IHPlotVal * This);


void __RPC_STUB IHPlotVal_RemoveAnnotationText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_SquarePlot_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbSquare);


void __RPC_STUB IHPlotVal_get_SquarePlot_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_SquarePlot_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbSquare);


void __RPC_STUB IHPlotVal_put_SquarePlot_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_CanLive_Proxy( 
    IHPlotVal * This,
    /* [retval][out] */ VARIANT_BOOL *pbCanLive);


void __RPC_STUB IHPlotVal_get_CanLive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_CanLive_Proxy( 
    IHPlotVal * This,
    /* [in] */ VARIANT_BOOL pbCanLive);


void __RPC_STUB IHPlotVal_put_CanLive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_get_AxisScale_Proxy( 
    IHPlotVal * This,
    /* [in] */ long VarNo,
    /* [retval][out] */ long *pScale);


void __RPC_STUB IHPlotVal_get_AxisScale_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHPlotVal_put_AxisScale_Proxy( 
    IHPlotVal * This,
    /* [in] */ long VarNo,
    /* [in] */ long pScale);


void __RPC_STUB IHPlotVal_put_AxisScale_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHPlotVal_INTERFACE_DEFINED__ */


#ifndef __IAPPDF_INTERFACE_DEFINED__
#define __IAPPDF_INTERFACE_DEFINED__

/* interface IAPPDF */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPDF;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("D6FECDE0-BA92-11D1-9B3F-0000C07EE8F2")
    IAPPDF : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetAPPDF( 
            /* [in] */ BSTR filename,
            /* [in] */ long flags) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPDFVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPDF * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPDF * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPDF * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPDF * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPDF * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPDF * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPDF * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetAPPDF )( 
            IAPPDF * This,
            /* [in] */ BSTR filename,
            /* [in] */ long flags);
        
        END_INTERFACE
    } IAPPDFVtbl;

    interface IAPPDF
    {
        CONST_VTBL struct IAPPDFVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPDF_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPDF_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPDF_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPDF_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPDF_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPDF_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPDF_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPDF_GetAPPDF(This,filename,flags)	\
    (This)->lpVtbl -> GetAPPDF(This,filename,flags)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IAPPDF_GetAPPDF_Proxy( 
    IAPPDF * This,
    /* [in] */ BSTR filename,
    /* [in] */ long flags);


void __RPC_STUB IAPPDF_GetAPPDF_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPDF_INTERFACE_DEFINED__ */


#ifndef __IHAPHandle_INTERFACE_DEFINED__
#define __IHAPHandle_INTERFACE_DEFINED__

/* interface IHAPHandle */
/* [object][oleautomation][dual][hidden][helpstring][uuid] */ 


EXTERN_C const IID IID_IHAPHandle;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("ADBB8C5B-2BC3-4320-80E9-1173394ED542")
    IHAPHandle : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MainHWnd( 
            /* [retval][out] */ long *lphWnd) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ProcessHandle( 
            /* [retval][out] */ long *lphproc) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHAPHandleVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHAPHandle * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHAPHandle * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHAPHandle * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHAPHandle * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHAPHandle * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHAPHandle * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHAPHandle * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MainHWnd )( 
            IHAPHandle * This,
            /* [retval][out] */ long *lphWnd);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ProcessHandle )( 
            IHAPHandle * This,
            /* [retval][out] */ long *lphproc);
        
        END_INTERFACE
    } IHAPHandleVtbl;

    interface IHAPHandle
    {
        CONST_VTBL struct IHAPHandleVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHAPHandle_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHAPHandle_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHAPHandle_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHAPHandle_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHAPHandle_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHAPHandle_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHAPHandle_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHAPHandle_get_MainHWnd(This,lphWnd)	\
    (This)->lpVtbl -> get_MainHWnd(This,lphWnd)

#define IHAPHandle_get_ProcessHandle(This,lphproc)	\
    (This)->lpVtbl -> get_ProcessHandle(This,lphproc)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPHandle_get_MainHWnd_Proxy( 
    IHAPHandle * This,
    /* [retval][out] */ long *lphWnd);


void __RPC_STUB IHAPHandle_get_MainHWnd_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAPHandle_get_ProcessHandle_Proxy( 
    IHAPHandle * This,
    /* [retval][out] */ long *lphproc);


void __RPC_STUB IHAPHandle_get_ProcessHandle_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHAPHandle_INTERFACE_DEFINED__ */


#ifndef __IAPropData_INTERFACE_DEFINED__
#define __IAPropData_INTERFACE_DEFINED__

/* interface IAPropData */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPropData;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("0B61E982-00B4-4308-994E-66C15D8F5CAC")
    IAPropData : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetAvailableDatabanks( 
            /* [out] */ long *plNumDatabanks,
            /* [out] */ VARIANT *pvtDatabankArray) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetDefaultDatabanks( 
            /* [out] */ long *plNumDatabanks,
            /* [out] */ VARIANT *pvtDatabankArray) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetQueryForComponents( 
            /* [in] */ VARIANT vtDatabanksArray,
            /* [in] */ BSTR bstrMatchNameAlias,
            /* [in] */ long bMatchAlternate,
            /* [in] */ long bMatchStringBeginOnly,
            /* [in] */ BSTR bstrCompClass,
            /* [in] */ BSTR bstrCASRN,
            /* [in] */ double dblMWfrom,
            /* [in] */ double dblMWto,
            /* [in] */ double dblBPfrom,
            /* [in] */ double dblBPto,
            /* [in] */ BSTR bstrBPUnit) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FetchNextComponent( 
            /* [in] */ long bInit,
            /* [out] */ BSTR *bstrAlias,
            /* [out] */ BSTR *bstrName,
            /* [out] */ double *dblBP,
            /* [out] */ double *dblMW,
            /* [out] */ BSTR *bstrDatabank,
            /* [out] */ BSTR *bstrCASRN,
            /* [out] */ BSTR *bstrCompClass,
            /* [out] */ BSTR *bstrSynonym) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPropDataVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPropData * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPropData * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPropData * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPropData * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPropData * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPropData * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPropData * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetAvailableDatabanks )( 
            IAPropData * This,
            /* [out] */ long *plNumDatabanks,
            /* [out] */ VARIANT *pvtDatabankArray);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetDefaultDatabanks )( 
            IAPropData * This,
            /* [out] */ long *plNumDatabanks,
            /* [out] */ VARIANT *pvtDatabankArray);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetQueryForComponents )( 
            IAPropData * This,
            /* [in] */ VARIANT vtDatabanksArray,
            /* [in] */ BSTR bstrMatchNameAlias,
            /* [in] */ long bMatchAlternate,
            /* [in] */ long bMatchStringBeginOnly,
            /* [in] */ BSTR bstrCompClass,
            /* [in] */ BSTR bstrCASRN,
            /* [in] */ double dblMWfrom,
            /* [in] */ double dblMWto,
            /* [in] */ double dblBPfrom,
            /* [in] */ double dblBPto,
            /* [in] */ BSTR bstrBPUnit);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FetchNextComponent )( 
            IAPropData * This,
            /* [in] */ long bInit,
            /* [out] */ BSTR *bstrAlias,
            /* [out] */ BSTR *bstrName,
            /* [out] */ double *dblBP,
            /* [out] */ double *dblMW,
            /* [out] */ BSTR *bstrDatabank,
            /* [out] */ BSTR *bstrCASRN,
            /* [out] */ BSTR *bstrCompClass,
            /* [out] */ BSTR *bstrSynonym);
        
        END_INTERFACE
    } IAPropDataVtbl;

    interface IAPropData
    {
        CONST_VTBL struct IAPropDataVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPropData_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPropData_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPropData_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPropData_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPropData_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPropData_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPropData_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPropData_GetAvailableDatabanks(This,plNumDatabanks,pvtDatabankArray)	\
    (This)->lpVtbl -> GetAvailableDatabanks(This,plNumDatabanks,pvtDatabankArray)

#define IAPropData_GetDefaultDatabanks(This,plNumDatabanks,pvtDatabankArray)	\
    (This)->lpVtbl -> GetDefaultDatabanks(This,plNumDatabanks,pvtDatabankArray)

#define IAPropData_SetQueryForComponents(This,vtDatabanksArray,bstrMatchNameAlias,bMatchAlternate,bMatchStringBeginOnly,bstrCompClass,bstrCASRN,dblMWfrom,dblMWto,dblBPfrom,dblBPto,bstrBPUnit)	\
    (This)->lpVtbl -> SetQueryForComponents(This,vtDatabanksArray,bstrMatchNameAlias,bMatchAlternate,bMatchStringBeginOnly,bstrCompClass,bstrCASRN,dblMWfrom,dblMWto,dblBPfrom,dblBPto,bstrBPUnit)

#define IAPropData_FetchNextComponent(This,bInit,bstrAlias,bstrName,dblBP,dblMW,bstrDatabank,bstrCASRN,bstrCompClass,bstrSynonym)	\
    (This)->lpVtbl -> FetchNextComponent(This,bInit,bstrAlias,bstrName,dblBP,dblMW,bstrDatabank,bstrCASRN,bstrCompClass,bstrSynonym)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IAPropData_GetAvailableDatabanks_Proxy( 
    IAPropData * This,
    /* [out] */ long *plNumDatabanks,
    /* [out] */ VARIANT *pvtDatabankArray);


void __RPC_STUB IAPropData_GetAvailableDatabanks_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IAPropData_GetDefaultDatabanks_Proxy( 
    IAPropData * This,
    /* [out] */ long *plNumDatabanks,
    /* [out] */ VARIANT *pvtDatabankArray);


void __RPC_STUB IAPropData_GetDefaultDatabanks_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IAPropData_SetQueryForComponents_Proxy( 
    IAPropData * This,
    /* [in] */ VARIANT vtDatabanksArray,
    /* [in] */ BSTR bstrMatchNameAlias,
    /* [in] */ long bMatchAlternate,
    /* [in] */ long bMatchStringBeginOnly,
    /* [in] */ BSTR bstrCompClass,
    /* [in] */ BSTR bstrCASRN,
    /* [in] */ double dblMWfrom,
    /* [in] */ double dblMWto,
    /* [in] */ double dblBPfrom,
    /* [in] */ double dblBPto,
    /* [in] */ BSTR bstrBPUnit);


void __RPC_STUB IAPropData_SetQueryForComponents_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IAPropData_FetchNextComponent_Proxy( 
    IAPropData * This,
    /* [in] */ long bInit,
    /* [out] */ BSTR *bstrAlias,
    /* [out] */ BSTR *bstrName,
    /* [out] */ double *dblBP,
    /* [out] */ double *dblMW,
    /* [out] */ BSTR *bstrDatabank,
    /* [out] */ BSTR *bstrCASRN,
    /* [out] */ BSTR *bstrCompClass,
    /* [out] */ BSTR *bstrSynonym);


void __RPC_STUB IAPropData_FetchNextComponent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPropData_INTERFACE_DEFINED__ */


#ifndef __IAPPasteItems_INTERFACE_DEFINED__
#define __IAPPasteItems_INTERFACE_DEFINED__

/* interface IAPPasteItems */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPasteItems;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2534-74A1-4094-9939-73276031FCB4")
    IAPPasteItems : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Item( 
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPPasteItem **pItem) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Enum( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *pCount) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPasteItemsVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPasteItems * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPasteItems * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPasteItems * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPasteItems * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPasteItems * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPasteItems * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPasteItems * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Item )( 
            IAPPasteItems * This,
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPPasteItem **pItem);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Enum )( 
            IAPPasteItems * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IAPPasteItems * This,
            /* [retval][out] */ long *pCount);
        
        END_INTERFACE
    } IAPPasteItemsVtbl;

    interface IAPPasteItems
    {
        CONST_VTBL struct IAPPasteItemsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPasteItems_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPasteItems_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPasteItems_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPasteItems_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPasteItems_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPasteItems_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPasteItems_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPasteItems_get_Item(This,index,pItem)	\
    (This)->lpVtbl -> get_Item(This,index,pItem)

#define IAPPasteItems_get_Enum(This,retval)	\
    (This)->lpVtbl -> get_Enum(This,retval)

#define IAPPasteItems_get_Count(This,pCount)	\
    (This)->lpVtbl -> get_Count(This,pCount)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItems_get_Item_Proxy( 
    IAPPasteItems * This,
    /* [in] */ VARIANT index,
    /* [retval][out] */ IAPPasteItem **pItem);


void __RPC_STUB IAPPasteItems_get_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItems_get_Enum_Proxy( 
    IAPPasteItems * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IAPPasteItems_get_Enum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItems_get_Count_Proxy( 
    IAPPasteItems * This,
    /* [retval][out] */ long *pCount);


void __RPC_STUB IAPPasteItems_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPasteItems_INTERFACE_DEFINED__ */


#ifndef __IAPPasteItem_INTERFACE_DEFINED__
#define __IAPPasteItem_INTERFACE_DEFINED__

/* interface IAPPasteItem */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPasteItem;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2533-74A1-4094-9939-73276031FCB4")
    IAPPasteItem : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_displayname( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_ID( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_ID( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Deleted( 
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Deleted( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_PathLen( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_path( 
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_HasChildren( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Items( 
            /* [retval][out] */ IAPPasteItems **retval) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPasteItemVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPasteItem * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPasteItem * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPasteItem * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPasteItem * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPasteItem * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPasteItem * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPasteItem * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_displayname )( 
            IAPPasteItem * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ID )( 
            IAPPasteItem * This,
            /* [in] */ BSTR Name);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ID )( 
            IAPPasteItem * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Deleted )( 
            IAPPasteItem * This,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Deleted )( 
            IAPPasteItem * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PathLen )( 
            IAPPasteItem * This,
            /* [retval][out] */ long *Value);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_path )( 
            IAPPasteItem * This,
            /* [in] */ long index,
            /* [retval][out] */ BSTR *Name);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasChildren )( 
            IAPPasteItem * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Items )( 
            IAPPasteItem * This,
            /* [retval][out] */ IAPPasteItems **retval);
        
        END_INTERFACE
    } IAPPasteItemVtbl;

    interface IAPPasteItem
    {
        CONST_VTBL struct IAPPasteItemVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPasteItem_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPasteItem_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPasteItem_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPasteItem_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPasteItem_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPasteItem_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPasteItem_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPasteItem_get_displayname(This,Name)	\
    (This)->lpVtbl -> get_displayname(This,Name)

#define IAPPasteItem_put_ID(This,Name)	\
    (This)->lpVtbl -> put_ID(This,Name)

#define IAPPasteItem_get_ID(This,Name)	\
    (This)->lpVtbl -> get_ID(This,Name)

#define IAPPasteItem_put_Deleted(This,retval)	\
    (This)->lpVtbl -> put_Deleted(This,retval)

#define IAPPasteItem_get_Deleted(This,retval)	\
    (This)->lpVtbl -> get_Deleted(This,retval)

#define IAPPasteItem_get_PathLen(This,Value)	\
    (This)->lpVtbl -> get_PathLen(This,Value)

#define IAPPasteItem_get_path(This,index,Name)	\
    (This)->lpVtbl -> get_path(This,index,Name)

#define IAPPasteItem_get_HasChildren(This,retval)	\
    (This)->lpVtbl -> get_HasChildren(This,retval)

#define IAPPasteItem_get_Items(This,retval)	\
    (This)->lpVtbl -> get_Items(This,retval)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_displayname_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteItem_get_displayname_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_put_ID_Proxy( 
    IAPPasteItem * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IAPPasteItem_put_ID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_ID_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteItem_get_ID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_put_Deleted_Proxy( 
    IAPPasteItem * This,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IAPPasteItem_put_Deleted_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_Deleted_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IAPPasteItem_get_Deleted_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_PathLen_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ long *Value);


void __RPC_STUB IAPPasteItem_get_PathLen_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_path_Proxy( 
    IAPPasteItem * This,
    /* [in] */ long index,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteItem_get_path_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_HasChildren_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IAPPasteItem_get_HasChildren_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteItem_get_Items_Proxy( 
    IAPPasteItem * This,
    /* [retval][out] */ IAPPasteItems **retval);


void __RPC_STUB IAPPasteItem_get_Items_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPasteItem_INTERFACE_DEFINED__ */


#ifndef __IHSelectionCallback_INTERFACE_DEFINED__
#define __IHSelectionCallback_INTERFACE_DEFINED__

/* interface IHSelectionCallback */
/* [object][hidden][helpstring][uuid] */ 


EXTERN_C const IID IID_IHSelectionCallback;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("3E9D8E11-DBF3-4A5E-8922-5D9A7A4A6EBD")
    IHSelectionCallback : public IUnknown
    {
    public:
        virtual /* [propget] */ HRESULT __stdcall get_Count( 
            /* [retval][out] */ int *pCount) = 0;
        
        virtual /* [helpstring] */ HRESULT __stdcall LoadSelection( 
            /* [in] */ IHSelection *arg1) = 0;
        
        virtual /* [propget] */ HRESULT __stdcall get_EditsAllowed( 
            /* [retval][out] */ int *Value) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHSelectionCallbackVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHSelectionCallback * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHSelectionCallback * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHSelectionCallback * This);
        
        /* [propget] */ HRESULT ( __stdcall *get_Count )( 
            IHSelectionCallback * This,
            /* [retval][out] */ int *pCount);
        
        /* [helpstring] */ HRESULT ( __stdcall *LoadSelection )( 
            IHSelectionCallback * This,
            /* [in] */ IHSelection *arg1);
        
        /* [propget] */ HRESULT ( __stdcall *get_EditsAllowed )( 
            IHSelectionCallback * This,
            /* [retval][out] */ int *Value);
        
        END_INTERFACE
    } IHSelectionCallbackVtbl;

    interface IHSelectionCallback
    {
        CONST_VTBL struct IHSelectionCallbackVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHSelectionCallback_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHSelectionCallback_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHSelectionCallback_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHSelectionCallback_get_Count(This,pCount)	\
    (This)->lpVtbl -> get_Count(This,pCount)

#define IHSelectionCallback_LoadSelection(This,arg1)	\
    (This)->lpVtbl -> LoadSelection(This,arg1)

#define IHSelectionCallback_get_EditsAllowed(This,Value)	\
    (This)->lpVtbl -> get_EditsAllowed(This,Value)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget] */ HRESULT __stdcall IHSelectionCallback_get_Count_Proxy( 
    IHSelectionCallback * This,
    /* [retval][out] */ int *pCount);


void __RPC_STUB IHSelectionCallback_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring] */ HRESULT __stdcall IHSelectionCallback_LoadSelection_Proxy( 
    IHSelectionCallback * This,
    /* [in] */ IHSelection *arg1);


void __RPC_STUB IHSelectionCallback_LoadSelection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget] */ HRESULT __stdcall IHSelectionCallback_get_EditsAllowed_Proxy( 
    IHSelectionCallback * This,
    /* [retval][out] */ int *Value);


void __RPC_STUB IHSelectionCallback_get_EditsAllowed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHSelectionCallback_INTERFACE_DEFINED__ */


#ifndef __IAPHappEvents_DISPINTERFACE_DEFINED__
#define __IAPHappEvents_DISPINTERFACE_DEFINED__

/* dispinterface IAPHappEvents */
/* [helpstring][uuid] */ 


EXTERN_C const IID DIID_IAPHappEvents;

#if defined(__cplusplus) && !defined(CINTERFACE)

    MIDL_INTERFACE("503FD632-F63E-11D1-8D3E-0000C033EAF2")
    IAPHappEvents : public IDispatch
    {
    };
    
#else 	/* C style interface */

    typedef struct IAPHappEventsVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPHappEvents * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPHappEvents * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPHappEvents * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPHappEvents * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPHappEvents * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPHappEvents * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPHappEvents * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } IAPHappEventsVtbl;

    interface IAPHappEvents
    {
        CONST_VTBL struct IAPHappEventsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPHappEvents_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPHappEvents_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPHappEvents_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPHappEvents_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPHappEvents_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPHappEvents_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPHappEvents_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)

#endif /* COBJMACROS */


#endif 	/* C style interface */


#endif 	/* __IAPHappEvents_DISPINTERFACE_DEFINED__ */


#ifndef __IHComposite_INTERFACE_DEFINED__
#define __IHComposite_INTERFACE_DEFINED__

/* interface IHComposite */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHComposite;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("72F9FD70-9ED4-11D0-9475-0000C07972E4")
    IHComposite : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddChild( 
            /* [in] */ BSTR Name,
            /* [in] */ IHNode *child) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveChild( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetChild( 
            /* [in] */ int index,
            /* [retval][out] */ IHNode **retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetChildName( 
            /* [in] */ int index,
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ChildCount( 
            /* [retval][out] */ long *Count) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DumpToDataBase( 
            /* [in] */ BSTR filename,
            /* [in] */ BSTR tablename,
            /* [in] */ int create) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHCompositeVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHComposite * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHComposite * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHComposite * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHComposite * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHComposite * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHComposite * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHComposite * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddChild )( 
            IHComposite * This,
            /* [in] */ BSTR Name,
            /* [in] */ IHNode *child);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveChild )( 
            IHComposite * This,
            /* [in] */ BSTR Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetChild )( 
            IHComposite * This,
            /* [in] */ int index,
            /* [retval][out] */ IHNode **retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetChildName )( 
            IHComposite * This,
            /* [in] */ int index,
            /* [retval][out] */ BSTR *Name);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ChildCount )( 
            IHComposite * This,
            /* [retval][out] */ long *Count);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DumpToDataBase )( 
            IHComposite * This,
            /* [in] */ BSTR filename,
            /* [in] */ BSTR tablename,
            /* [in] */ int create);
        
        END_INTERFACE
    } IHCompositeVtbl;

    interface IHComposite
    {
        CONST_VTBL struct IHCompositeVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHComposite_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHComposite_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHComposite_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHComposite_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHComposite_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHComposite_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHComposite_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHComposite_AddChild(This,Name,child)	\
    (This)->lpVtbl -> AddChild(This,Name,child)

#define IHComposite_RemoveChild(This,Name)	\
    (This)->lpVtbl -> RemoveChild(This,Name)

#define IHComposite_GetChild(This,index,retval)	\
    (This)->lpVtbl -> GetChild(This,index,retval)

#define IHComposite_GetChildName(This,index,Name)	\
    (This)->lpVtbl -> GetChildName(This,index,Name)

#define IHComposite_get_ChildCount(This,Count)	\
    (This)->lpVtbl -> get_ChildCount(This,Count)

#define IHComposite_DumpToDataBase(This,filename,tablename,create)	\
    (This)->lpVtbl -> DumpToDataBase(This,filename,tablename,create)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHComposite_AddChild_Proxy( 
    IHComposite * This,
    /* [in] */ BSTR Name,
    /* [in] */ IHNode *child);


void __RPC_STUB IHComposite_AddChild_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHComposite_RemoveChild_Proxy( 
    IHComposite * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IHComposite_RemoveChild_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHComposite_GetChild_Proxy( 
    IHComposite * This,
    /* [in] */ int index,
    /* [retval][out] */ IHNode **retval);


void __RPC_STUB IHComposite_GetChild_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHComposite_GetChildName_Proxy( 
    IHComposite * This,
    /* [in] */ int index,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IHComposite_GetChildName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHComposite_get_ChildCount_Proxy( 
    IHComposite * This,
    /* [retval][out] */ long *Count);


void __RPC_STUB IHComposite_get_ChildCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHComposite_DumpToDataBase_Proxy( 
    IHComposite * This,
    /* [in] */ BSTR filename,
    /* [in] */ BSTR tablename,
    /* [in] */ int create);


void __RPC_STUB IHComposite_DumpToDataBase_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHComposite_INTERFACE_DEFINED__ */


#ifndef __IHWizardPlot_INTERFACE_DEFINED__
#define __IHWizardPlot_INTERFACE_DEFINED__

/* interface IHWizardPlot */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHWizardPlot;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("8008C730-1A3E-11D1-8A4D-0000C0237DF9")
    IHWizardPlot : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PlotVal( 
            /* [retval][out] */ IHPlotVal **pPlotVal) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_basis( 
            /* [retval][out] */ long *basis) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_basis( 
            /* [in] */ long basis) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_unitrow( 
            /* [optional][in] */ VARIANT curve,
            /* [retval][out] */ long *unitrow) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_unitrow( 
            /* [optional][in] */ VARIANT curve,
            /* [in] */ long unitrow) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_unitcol( 
            /* [optional][in] */ VARIANT curve,
            /* [retval][out] */ long *unitcol) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_unitcol( 
            /* [optional][in] */ VARIANT curve,
            /* [in] */ long unitcol) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetIndependentVariable( 
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR indepvar) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddDependentVariable( 
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveDependentVariable( 
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE RemoveAllDependentVariable( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasDependentVariable( 
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar,
            /* [retval][out] */ VARIANT_BOOL *pbVal) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddVariablePair( 
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR indepvar,
            /* [in] */ BSTR depvar,
            /* [in] */ short sort) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WizardData( 
            /* [in] */ long Step,
            /* [retval][out] */ IDispatch **ppData) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_WizardData( 
            /* [in] */ long Step,
            /* [in] */ IDispatch *ppData) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ErrorNumber( 
            /* [retval][out] */ long *pIndex) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ErrorStatus( 
            /* [in] */ long index,
            /* [in] */ VARIANT_BOOL rhs) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RefreshCallBack( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RefreshCallBack( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetIndependentVariable( 
            /* [out] */ BSTR *indepvar) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetDependentVariable( 
            /* [out][in] */ SAFEARRAY * *depvar) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetPlotList( 
            /* [out][in] */ SAFEARRAY * *plots) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AddToPlot( 
            /* [in] */ IHPlotVal *ppPlot) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AddToPlot( 
            /* [retval][out] */ IHPlotVal **ppPlot) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XUnitRow( 
            /* [retval][out] */ long *unitrow) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XUnitCol( 
            /* [retval][out] */ long *unitcol) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_XUnitCol( 
            /* [in] */ long unitcol) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHWizardPlotVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHWizardPlot * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHWizardPlot * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHWizardPlot * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHWizardPlot * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHWizardPlot * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHWizardPlot * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHWizardPlot * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PlotVal )( 
            IHWizardPlot * This,
            /* [retval][out] */ IHPlotVal **pPlotVal);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_basis )( 
            IHWizardPlot * This,
            /* [retval][out] */ long *basis);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_basis )( 
            IHWizardPlot * This,
            /* [in] */ long basis);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_unitrow )( 
            IHWizardPlot * This,
            /* [optional][in] */ VARIANT curve,
            /* [retval][out] */ long *unitrow);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_unitrow )( 
            IHWizardPlot * This,
            /* [optional][in] */ VARIANT curve,
            /* [in] */ long unitrow);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_unitcol )( 
            IHWizardPlot * This,
            /* [optional][in] */ VARIANT curve,
            /* [retval][out] */ long *unitcol);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_unitcol )( 
            IHWizardPlot * This,
            /* [optional][in] */ VARIANT curve,
            /* [in] */ long unitcol);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetIndependentVariable )( 
            IHWizardPlot * This,
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR indepvar);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddDependentVariable )( 
            IHWizardPlot * This,
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveDependentVariable )( 
            IHWizardPlot * This,
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *RemoveAllDependentVariable )( 
            IHWizardPlot * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasDependentVariable )( 
            IHWizardPlot * This,
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR depvar,
            /* [retval][out] */ VARIANT_BOOL *pbVal);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddVariablePair )( 
            IHWizardPlot * This,
            /* [in] */ IHNode *pContext,
            /* [in] */ BSTR indepvar,
            /* [in] */ BSTR depvar,
            /* [in] */ short sort);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WizardData )( 
            IHWizardPlot * This,
            /* [in] */ long Step,
            /* [retval][out] */ IDispatch **ppData);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_WizardData )( 
            IHWizardPlot * This,
            /* [in] */ long Step,
            /* [in] */ IDispatch *ppData);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IHWizardPlot * This);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ErrorNumber )( 
            IHWizardPlot * This,
            /* [retval][out] */ long *pIndex);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ErrorStatus )( 
            IHWizardPlot * This,
            /* [in] */ long index,
            /* [in] */ VARIANT_BOOL rhs);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RefreshCallBack )( 
            IHWizardPlot * This,
            /* [in] */ BSTR Name);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RefreshCallBack )( 
            IHWizardPlot * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetIndependentVariable )( 
            IHWizardPlot * This,
            /* [out] */ BSTR *indepvar);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetDependentVariable )( 
            IHWizardPlot * This,
            /* [out][in] */ SAFEARRAY * *depvar);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetPlotList )( 
            IHWizardPlot * This,
            /* [out][in] */ SAFEARRAY * *plots);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AddToPlot )( 
            IHWizardPlot * This,
            /* [in] */ IHPlotVal *ppPlot);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AddToPlot )( 
            IHWizardPlot * This,
            /* [retval][out] */ IHPlotVal **ppPlot);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XUnitRow )( 
            IHWizardPlot * This,
            /* [retval][out] */ long *unitrow);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XUnitCol )( 
            IHWizardPlot * This,
            /* [retval][out] */ long *unitcol);
        
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_XUnitCol )( 
            IHWizardPlot * This,
            /* [in] */ long unitcol);
        
        END_INTERFACE
    } IHWizardPlotVtbl;

    interface IHWizardPlot
    {
        CONST_VTBL struct IHWizardPlotVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHWizardPlot_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHWizardPlot_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHWizardPlot_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHWizardPlot_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHWizardPlot_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHWizardPlot_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHWizardPlot_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHWizardPlot_get_PlotVal(This,pPlotVal)	\
    (This)->lpVtbl -> get_PlotVal(This,pPlotVal)

#define IHWizardPlot_get_basis(This,basis)	\
    (This)->lpVtbl -> get_basis(This,basis)

#define IHWizardPlot_put_basis(This,basis)	\
    (This)->lpVtbl -> put_basis(This,basis)

#define IHWizardPlot_get_unitrow(This,curve,unitrow)	\
    (This)->lpVtbl -> get_unitrow(This,curve,unitrow)

#define IHWizardPlot_put_unitrow(This,curve,unitrow)	\
    (This)->lpVtbl -> put_unitrow(This,curve,unitrow)

#define IHWizardPlot_get_unitcol(This,curve,unitcol)	\
    (This)->lpVtbl -> get_unitcol(This,curve,unitcol)

#define IHWizardPlot_put_unitcol(This,curve,unitcol)	\
    (This)->lpVtbl -> put_unitcol(This,curve,unitcol)

#define IHWizardPlot_SetIndependentVariable(This,pContext,indepvar)	\
    (This)->lpVtbl -> SetIndependentVariable(This,pContext,indepvar)

#define IHWizardPlot_AddDependentVariable(This,pContext,depvar)	\
    (This)->lpVtbl -> AddDependentVariable(This,pContext,depvar)

#define IHWizardPlot_RemoveDependentVariable(This,pContext,depvar)	\
    (This)->lpVtbl -> RemoveDependentVariable(This,pContext,depvar)

#define IHWizardPlot_RemoveAllDependentVariable(This)	\
    (This)->lpVtbl -> RemoveAllDependentVariable(This)

#define IHWizardPlot_get_HasDependentVariable(This,pContext,depvar,pbVal)	\
    (This)->lpVtbl -> get_HasDependentVariable(This,pContext,depvar,pbVal)

#define IHWizardPlot_AddVariablePair(This,pContext,indepvar,depvar,sort)	\
    (This)->lpVtbl -> AddVariablePair(This,pContext,indepvar,depvar,sort)

#define IHWizardPlot_get_WizardData(This,Step,ppData)	\
    (This)->lpVtbl -> get_WizardData(This,Step,ppData)

#define IHWizardPlot_put_WizardData(This,Step,ppData)	\
    (This)->lpVtbl -> put_WizardData(This,Step,ppData)

#define IHWizardPlot_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IHWizardPlot_get_ErrorNumber(This,pIndex)	\
    (This)->lpVtbl -> get_ErrorNumber(This,pIndex)

#define IHWizardPlot_put_ErrorStatus(This,index,rhs)	\
    (This)->lpVtbl -> put_ErrorStatus(This,index,rhs)

#define IHWizardPlot_put_RefreshCallBack(This,Name)	\
    (This)->lpVtbl -> put_RefreshCallBack(This,Name)

#define IHWizardPlot_get_RefreshCallBack(This,Name)	\
    (This)->lpVtbl -> get_RefreshCallBack(This,Name)

#define IHWizardPlot_GetIndependentVariable(This,indepvar)	\
    (This)->lpVtbl -> GetIndependentVariable(This,indepvar)

#define IHWizardPlot_GetDependentVariable(This,depvar)	\
    (This)->lpVtbl -> GetDependentVariable(This,depvar)

#define IHWizardPlot_GetPlotList(This,plots)	\
    (This)->lpVtbl -> GetPlotList(This,plots)

#define IHWizardPlot_put_AddToPlot(This,ppPlot)	\
    (This)->lpVtbl -> put_AddToPlot(This,ppPlot)

#define IHWizardPlot_get_AddToPlot(This,ppPlot)	\
    (This)->lpVtbl -> get_AddToPlot(This,ppPlot)

#define IHWizardPlot_get_XUnitRow(This,unitrow)	\
    (This)->lpVtbl -> get_XUnitRow(This,unitrow)

#define IHWizardPlot_get_XUnitCol(This,unitcol)	\
    (This)->lpVtbl -> get_XUnitCol(This,unitcol)

#define IHWizardPlot_put_XUnitCol(This,unitcol)	\
    (This)->lpVtbl -> put_XUnitCol(This,unitcol)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_PlotVal_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ IHPlotVal **pPlotVal);


void __RPC_STUB IHWizardPlot_get_PlotVal_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_basis_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ long *basis);


void __RPC_STUB IHWizardPlot_get_basis_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_basis_Proxy( 
    IHWizardPlot * This,
    /* [in] */ long basis);


void __RPC_STUB IHWizardPlot_put_basis_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_unitrow_Proxy( 
    IHWizardPlot * This,
    /* [optional][in] */ VARIANT curve,
    /* [retval][out] */ long *unitrow);


void __RPC_STUB IHWizardPlot_get_unitrow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_unitrow_Proxy( 
    IHWizardPlot * This,
    /* [optional][in] */ VARIANT curve,
    /* [in] */ long unitrow);


void __RPC_STUB IHWizardPlot_put_unitrow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_unitcol_Proxy( 
    IHWizardPlot * This,
    /* [optional][in] */ VARIANT curve,
    /* [retval][out] */ long *unitcol);


void __RPC_STUB IHWizardPlot_get_unitcol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_unitcol_Proxy( 
    IHWizardPlot * This,
    /* [optional][in] */ VARIANT curve,
    /* [in] */ long unitcol);


void __RPC_STUB IHWizardPlot_put_unitcol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_SetIndependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHNode *pContext,
    /* [in] */ BSTR indepvar);


void __RPC_STUB IHWizardPlot_SetIndependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_AddDependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHNode *pContext,
    /* [in] */ BSTR depvar);


void __RPC_STUB IHWizardPlot_AddDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_RemoveDependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHNode *pContext,
    /* [in] */ BSTR depvar);


void __RPC_STUB IHWizardPlot_RemoveDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_RemoveAllDependentVariable_Proxy( 
    IHWizardPlot * This);


void __RPC_STUB IHWizardPlot_RemoveAllDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_HasDependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHNode *pContext,
    /* [in] */ BSTR depvar,
    /* [retval][out] */ VARIANT_BOOL *pbVal);


void __RPC_STUB IHWizardPlot_get_HasDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_AddVariablePair_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHNode *pContext,
    /* [in] */ BSTR indepvar,
    /* [in] */ BSTR depvar,
    /* [in] */ short sort);


void __RPC_STUB IHWizardPlot_AddVariablePair_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_WizardData_Proxy( 
    IHWizardPlot * This,
    /* [in] */ long Step,
    /* [retval][out] */ IDispatch **ppData);


void __RPC_STUB IHWizardPlot_get_WizardData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_WizardData_Proxy( 
    IHWizardPlot * This,
    /* [in] */ long Step,
    /* [in] */ IDispatch *ppData);


void __RPC_STUB IHWizardPlot_put_WizardData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_Clear_Proxy( 
    IHWizardPlot * This);


void __RPC_STUB IHWizardPlot_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_ErrorNumber_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ long *pIndex);


void __RPC_STUB IHWizardPlot_get_ErrorNumber_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_ErrorStatus_Proxy( 
    IHWizardPlot * This,
    /* [in] */ long index,
    /* [in] */ VARIANT_BOOL rhs);


void __RPC_STUB IHWizardPlot_put_ErrorStatus_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_RefreshCallBack_Proxy( 
    IHWizardPlot * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IHWizardPlot_put_RefreshCallBack_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_RefreshCallBack_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IHWizardPlot_get_RefreshCallBack_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_GetIndependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [out] */ BSTR *indepvar);


void __RPC_STUB IHWizardPlot_GetIndependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_GetDependentVariable_Proxy( 
    IHWizardPlot * This,
    /* [out][in] */ SAFEARRAY * *depvar);


void __RPC_STUB IHWizardPlot_GetDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_GetPlotList_Proxy( 
    IHWizardPlot * This,
    /* [out][in] */ SAFEARRAY * *plots);


void __RPC_STUB IHWizardPlot_GetPlotList_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_AddToPlot_Proxy( 
    IHWizardPlot * This,
    /* [in] */ IHPlotVal *ppPlot);


void __RPC_STUB IHWizardPlot_put_AddToPlot_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_AddToPlot_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ IHPlotVal **ppPlot);


void __RPC_STUB IHWizardPlot_get_AddToPlot_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_XUnitRow_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ long *unitrow);


void __RPC_STUB IHWizardPlot_get_XUnitRow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_get_XUnitCol_Proxy( 
    IHWizardPlot * This,
    /* [retval][out] */ long *unitcol);


void __RPC_STUB IHWizardPlot_get_XUnitCol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE IHWizardPlot_put_XUnitCol_Proxy( 
    IHWizardPlot * This,
    /* [in] */ long unitcol);


void __RPC_STUB IHWizardPlot_put_XUnitCol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHWizardPlot_INTERFACE_DEFINED__ */


#ifndef __IHAdhocPlot_INTERFACE_DEFINED__
#define __IHAdhocPlot_INTERFACE_DEFINED__

/* interface IHAdhocPlot */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHAdhocPlot;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("623A7830-34F0-11D1-8A58-0000C0237DF9")
    IHAdhocPlot : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PlotVal( 
            /* [retval][out] */ IHPlotVal **pPlotVal) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ReadyToPlot( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetIndependentVariable( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetDependentVariable( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetParametricVariable( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Refresh( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHAdhocPlotVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHAdhocPlot * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHAdhocPlot * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHAdhocPlot * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHAdhocPlot * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHAdhocPlot * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHAdhocPlot * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHAdhocPlot * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PlotVal )( 
            IHAdhocPlot * This,
            /* [retval][out] */ IHPlotVal **pPlotVal);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ReadyToPlot )( 
            IHAdhocPlot * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetIndependentVariable )( 
            IHAdhocPlot * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetDependentVariable )( 
            IHAdhocPlot * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetParametricVariable )( 
            IHAdhocPlot * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Refresh )( 
            IHAdhocPlot * This);
        
        END_INTERFACE
    } IHAdhocPlotVtbl;

    interface IHAdhocPlot
    {
        CONST_VTBL struct IHAdhocPlotVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHAdhocPlot_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHAdhocPlot_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHAdhocPlot_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHAdhocPlot_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHAdhocPlot_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHAdhocPlot_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHAdhocPlot_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHAdhocPlot_get_PlotVal(This,pPlotVal)	\
    (This)->lpVtbl -> get_PlotVal(This,pPlotVal)

#define IHAdhocPlot_ReadyToPlot(This)	\
    (This)->lpVtbl -> ReadyToPlot(This)

#define IHAdhocPlot_SetIndependentVariable(This)	\
    (This)->lpVtbl -> SetIndependentVariable(This)

#define IHAdhocPlot_SetDependentVariable(This)	\
    (This)->lpVtbl -> SetDependentVariable(This)

#define IHAdhocPlot_SetParametricVariable(This)	\
    (This)->lpVtbl -> SetParametricVariable(This)

#define IHAdhocPlot_Refresh(This)	\
    (This)->lpVtbl -> Refresh(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_get_PlotVal_Proxy( 
    IHAdhocPlot * This,
    /* [retval][out] */ IHPlotVal **pPlotVal);


void __RPC_STUB IHAdhocPlot_get_PlotVal_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_ReadyToPlot_Proxy( 
    IHAdhocPlot * This);


void __RPC_STUB IHAdhocPlot_ReadyToPlot_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_SetIndependentVariable_Proxy( 
    IHAdhocPlot * This);


void __RPC_STUB IHAdhocPlot_SetIndependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_SetDependentVariable_Proxy( 
    IHAdhocPlot * This);


void __RPC_STUB IHAdhocPlot_SetDependentVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_SetParametricVariable_Proxy( 
    IHAdhocPlot * This);


void __RPC_STUB IHAdhocPlot_SetParametricVariable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHAdhocPlot_Refresh_Proxy( 
    IHAdhocPlot * This);


void __RPC_STUB IHAdhocPlot_Refresh_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHAdhocPlot_INTERFACE_DEFINED__ */


#ifndef __IMMControlVerb_INTERFACE_DEFINED__
#define __IMMControlVerb_INTERFACE_DEFINED__

/* interface IMMControlVerb */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IMMControlVerb;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("F9688B90-65A9-11D1-8A9D-0000C0237DF9")
    IMMControlVerb : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE EnumVerbs( 
            /* [out][in] */ VARIANT *pEnumOleVerb) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoVerb( 
            /* [in] */ long iVerb) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IMMControlVerbVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IMMControlVerb * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IMMControlVerb * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IMMControlVerb * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IMMControlVerb * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IMMControlVerb * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IMMControlVerb * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IMMControlVerb * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *EnumVerbs )( 
            IMMControlVerb * This,
            /* [out][in] */ VARIANT *pEnumOleVerb);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoVerb )( 
            IMMControlVerb * This,
            /* [in] */ long iVerb);
        
        END_INTERFACE
    } IMMControlVerbVtbl;

    interface IMMControlVerb
    {
        CONST_VTBL struct IMMControlVerbVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IMMControlVerb_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMMControlVerb_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IMMControlVerb_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IMMControlVerb_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IMMControlVerb_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IMMControlVerb_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IMMControlVerb_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IMMControlVerb_EnumVerbs(This,pEnumOleVerb)	\
    (This)->lpVtbl -> EnumVerbs(This,pEnumOleVerb)

#define IMMControlVerb_DoVerb(This,iVerb)	\
    (This)->lpVtbl -> DoVerb(This,iVerb)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IMMControlVerb_EnumVerbs_Proxy( 
    IMMControlVerb * This,
    /* [out][in] */ VARIANT *pEnumOleVerb);


void __RPC_STUB IMMControlVerb_EnumVerbs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IMMControlVerb_DoVerb_Proxy( 
    IMMControlVerb * This,
    /* [in] */ long iVerb);


void __RPC_STUB IMMControlVerb_DoVerb_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IMMControlVerb_INTERFACE_DEFINED__ */


#ifndef __IHappServiceProvider_INTERFACE_DEFINED__
#define __IHappServiceProvider_INTERFACE_DEFINED__

/* interface IHappServiceProvider */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHappServiceProvider;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("F98A1533-D1C7-4222-903C-87E3D47589AA")
    IHappServiceProvider : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetService( 
            /* [in] */ BSTR bService,
            /* [retval][out] */ IUnknown **ppUnk) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHappServiceProviderVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHappServiceProvider * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHappServiceProvider * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHappServiceProvider * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHappServiceProvider * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHappServiceProvider * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHappServiceProvider * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHappServiceProvider * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetService )( 
            IHappServiceProvider * This,
            /* [in] */ BSTR bService,
            /* [retval][out] */ IUnknown **ppUnk);
        
        END_INTERFACE
    } IHappServiceProviderVtbl;

    interface IHappServiceProvider
    {
        CONST_VTBL struct IHappServiceProviderVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHappServiceProvider_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHappServiceProvider_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHappServiceProvider_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHappServiceProvider_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHappServiceProvider_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHappServiceProvider_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHappServiceProvider_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHappServiceProvider_GetService(This,bService,ppUnk)	\
    (This)->lpVtbl -> GetService(This,bService,ppUnk)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHappServiceProvider_GetService_Proxy( 
    IHappServiceProvider * This,
    /* [in] */ BSTR bService,
    /* [retval][out] */ IUnknown **ppUnk);


void __RPC_STUB IHappServiceProvider_GetService_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHappServiceProvider_INTERFACE_DEFINED__ */


#ifndef __IAPConflict_INTERFACE_DEFINED__
#define __IAPConflict_INTERFACE_DEFINED__

/* interface IAPConflict */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPConflict;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2530-74A1-4094-9939-73276031FCB4")
    IAPConflict : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_displayname( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_ID( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_ID( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Merge( 
            /* [in] */ VARIANT_BOOL retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Merge( 
            /* [retval][out] */ VARIANT_BOOL *retval) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Action( 
            /* [in] */ IAP_CONFLICT_OPTION retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Action( 
            /* [retval][out] */ IAP_CONFLICT_OPTION *retval) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPConflictVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPConflict * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPConflict * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPConflict * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPConflict * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPConflict * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPConflict * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPConflict * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_displayname )( 
            IAPConflict * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ID )( 
            IAPConflict * This,
            /* [in] */ BSTR Name);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ID )( 
            IAPConflict * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Merge )( 
            IAPConflict * This,
            /* [in] */ VARIANT_BOOL retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Merge )( 
            IAPConflict * This,
            /* [retval][out] */ VARIANT_BOOL *retval);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Action )( 
            IAPConflict * This,
            /* [in] */ IAP_CONFLICT_OPTION retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Action )( 
            IAPConflict * This,
            /* [retval][out] */ IAP_CONFLICT_OPTION *retval);
        
        END_INTERFACE
    } IAPConflictVtbl;

    interface IAPConflict
    {
        CONST_VTBL struct IAPConflictVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPConflict_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPConflict_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPConflict_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPConflict_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPConflict_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPConflict_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPConflict_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPConflict_get_displayname(This,Name)	\
    (This)->lpVtbl -> get_displayname(This,Name)

#define IAPConflict_put_ID(This,Name)	\
    (This)->lpVtbl -> put_ID(This,Name)

#define IAPConflict_get_ID(This,Name)	\
    (This)->lpVtbl -> get_ID(This,Name)

#define IAPConflict_put_Merge(This,retval)	\
    (This)->lpVtbl -> put_Merge(This,retval)

#define IAPConflict_get_Merge(This,retval)	\
    (This)->lpVtbl -> get_Merge(This,retval)

#define IAPConflict_put_Action(This,retval)	\
    (This)->lpVtbl -> put_Action(This,retval)

#define IAPConflict_get_Action(This,retval)	\
    (This)->lpVtbl -> get_Action(This,retval)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_get_displayname_Proxy( 
    IAPConflict * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPConflict_get_displayname_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_put_ID_Proxy( 
    IAPConflict * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IAPConflict_put_ID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_get_ID_Proxy( 
    IAPConflict * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPConflict_get_ID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_put_Merge_Proxy( 
    IAPConflict * This,
    /* [in] */ VARIANT_BOOL retval);


void __RPC_STUB IAPConflict_put_Merge_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_get_Merge_Proxy( 
    IAPConflict * This,
    /* [retval][out] */ VARIANT_BOOL *retval);


void __RPC_STUB IAPConflict_get_Merge_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_put_Action_Proxy( 
    IAPConflict * This,
    /* [in] */ IAP_CONFLICT_OPTION retval);


void __RPC_STUB IAPConflict_put_Action_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflict_get_Action_Proxy( 
    IAPConflict * This,
    /* [retval][out] */ IAP_CONFLICT_OPTION *retval);


void __RPC_STUB IAPConflict_get_Action_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPConflict_INTERFACE_DEFINED__ */


#ifndef __IAPConflicts_INTERFACE_DEFINED__
#define __IAPConflicts_INTERFACE_DEFINED__

/* interface IAPConflicts */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPConflicts;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2531-74A1-4094-9939-73276031FCB4")
    IAPConflicts : public IDispatch
    {
    public:
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Item( 
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPConflict **ppConflict) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Enum( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *pCount) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPConflictsVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPConflicts * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPConflicts * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPConflicts * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPConflicts * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPConflicts * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPConflicts * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPConflicts * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Item )( 
            IAPConflicts * This,
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPConflict **ppConflict);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Enum )( 
            IAPConflicts * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IAPConflicts * This);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IAPConflicts * This,
            /* [retval][out] */ long *pCount);
        
        END_INTERFACE
    } IAPConflictsVtbl;

    interface IAPConflicts
    {
        CONST_VTBL struct IAPConflictsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPConflicts_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPConflicts_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPConflicts_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPConflicts_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPConflicts_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPConflicts_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPConflicts_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPConflicts_Item(This,index,ppConflict)	\
    (This)->lpVtbl -> Item(This,index,ppConflict)

#define IAPConflicts_get_Enum(This,retval)	\
    (This)->lpVtbl -> get_Enum(This,retval)

#define IAPConflicts_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IAPConflicts_get_Count(This,pCount)	\
    (This)->lpVtbl -> get_Count(This,pCount)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [id] */ HRESULT STDMETHODCALLTYPE IAPConflicts_Item_Proxy( 
    IAPConflicts * This,
    /* [in] */ VARIANT index,
    /* [retval][out] */ IAPConflict **ppConflict);


void __RPC_STUB IAPConflicts_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflicts_get_Enum_Proxy( 
    IAPConflicts * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IAPConflicts_get_Enum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPConflicts_Clear_Proxy( 
    IAPConflicts * This);


void __RPC_STUB IAPConflicts_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPConflicts_get_Count_Proxy( 
    IAPConflicts * This,
    /* [retval][out] */ long *pCount);


void __RPC_STUB IAPConflicts_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPConflicts_INTERFACE_DEFINED__ */


#ifndef __IAPGroup_INTERFACE_DEFINED__
#define __IAPGroup_INTERFACE_DEFINED__

/* interface IAPGroup */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPGroup;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2532-74A1-4094-9939-73276031FCB4")
    IAPGroup : public IDispatch
    {
    public:
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Add( 
            /* [in] */ VARIANT Item) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Remove( 
            /* [in] */ VARIANT Item) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Item( 
            /* [in] */ VARIANT index,
            /* [retval][out] */ VARIANT *pName) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get__NewEnum( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *pCount) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_DataObject( 
            /* [retval][out] */ IUnknown **pDataObject) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Export( 
            /* [defaultvalue][optional][in] */ BSTR filename = L"") = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE CopyToClipboard( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPGroupVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPGroup * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPGroup * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPGroup * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPGroup * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPGroup * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPGroup * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPGroup * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IAPGroup * This);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Add )( 
            IAPGroup * This,
            /* [in] */ VARIANT Item);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Remove )( 
            IAPGroup * This,
            /* [in] */ VARIANT Item);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Item )( 
            IAPGroup * This,
            /* [in] */ VARIANT index,
            /* [retval][out] */ VARIANT *pName);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get__NewEnum )( 
            IAPGroup * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IAPGroup * This,
            /* [retval][out] */ long *pCount);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DataObject )( 
            IAPGroup * This,
            /* [retval][out] */ IUnknown **pDataObject);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Export )( 
            IAPGroup * This,
            /* [defaultvalue][optional][in] */ BSTR filename);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *CopyToClipboard )( 
            IAPGroup * This);
        
        END_INTERFACE
    } IAPGroupVtbl;

    interface IAPGroup
    {
        CONST_VTBL struct IAPGroupVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPGroup_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPGroup_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPGroup_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPGroup_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPGroup_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPGroup_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPGroup_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPGroup_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IAPGroup_Add(This,Item)	\
    (This)->lpVtbl -> Add(This,Item)

#define IAPGroup_Remove(This,Item)	\
    (This)->lpVtbl -> Remove(This,Item)

#define IAPGroup_Item(This,index,pName)	\
    (This)->lpVtbl -> Item(This,index,pName)

#define IAPGroup_get__NewEnum(This,retval)	\
    (This)->lpVtbl -> get__NewEnum(This,retval)

#define IAPGroup_get_Count(This,pCount)	\
    (This)->lpVtbl -> get_Count(This,pCount)

#define IAPGroup_get_DataObject(This,pDataObject)	\
    (This)->lpVtbl -> get_DataObject(This,pDataObject)

#define IAPGroup_Export(This,filename)	\
    (This)->lpVtbl -> Export(This,filename)

#define IAPGroup_CopyToClipboard(This)	\
    (This)->lpVtbl -> CopyToClipboard(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_Clear_Proxy( 
    IAPGroup * This);


void __RPC_STUB IAPGroup_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_Add_Proxy( 
    IAPGroup * This,
    /* [in] */ VARIANT Item);


void __RPC_STUB IAPGroup_Add_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_Remove_Proxy( 
    IAPGroup * This,
    /* [in] */ VARIANT Item);


void __RPC_STUB IAPGroup_Remove_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_Item_Proxy( 
    IAPGroup * This,
    /* [in] */ VARIANT index,
    /* [retval][out] */ VARIANT *pName);


void __RPC_STUB IAPGroup_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPGroup_get__NewEnum_Proxy( 
    IAPGroup * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IAPGroup_get__NewEnum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPGroup_get_Count_Proxy( 
    IAPGroup * This,
    /* [retval][out] */ long *pCount);


void __RPC_STUB IAPGroup_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPGroup_get_DataObject_Proxy( 
    IAPGroup * This,
    /* [retval][out] */ IUnknown **pDataObject);


void __RPC_STUB IAPGroup_get_DataObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_Export_Proxy( 
    IAPGroup * This,
    /* [defaultvalue][optional][in] */ BSTR filename);


void __RPC_STUB IAPGroup_Export_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPGroup_CopyToClipboard_Proxy( 
    IAPGroup * This);


void __RPC_STUB IAPGroup_CopyToClipboard_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPGroup_INTERFACE_DEFINED__ */


#ifndef __IAPPasteParam_INTERFACE_DEFINED__
#define __IAPPasteParam_INTERFACE_DEFINED__

/* interface IAPPasteParam */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPasteParam;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2535-74A1-4094-9939-73276031FCB4")
    IAPPasteParam : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_ID( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Label( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Label( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Value( 
            /* [in] */ BSTR Name) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Value( 
            /* [retval][out] */ BSTR *Name) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPasteParamVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPasteParam * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPasteParam * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPasteParam * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPasteParam * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPasteParam * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPasteParam * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPasteParam * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ID )( 
            IAPPasteParam * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Label )( 
            IAPPasteParam * This,
            /* [in] */ BSTR Name);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Label )( 
            IAPPasteParam * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Value )( 
            IAPPasteParam * This,
            /* [in] */ BSTR Name);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Value )( 
            IAPPasteParam * This,
            /* [retval][out] */ BSTR *Name);
        
        END_INTERFACE
    } IAPPasteParamVtbl;

    interface IAPPasteParam
    {
        CONST_VTBL struct IAPPasteParamVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPasteParam_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPasteParam_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPasteParam_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPasteParam_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPasteParam_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPasteParam_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPasteParam_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPasteParam_get_ID(This,Name)	\
    (This)->lpVtbl -> get_ID(This,Name)

#define IAPPasteParam_put_Label(This,Name)	\
    (This)->lpVtbl -> put_Label(This,Name)

#define IAPPasteParam_get_Label(This,Name)	\
    (This)->lpVtbl -> get_Label(This,Name)

#define IAPPasteParam_put_Value(This,Name)	\
    (This)->lpVtbl -> put_Value(This,Name)

#define IAPPasteParam_get_Value(This,Name)	\
    (This)->lpVtbl -> get_Value(This,Name)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParam_get_ID_Proxy( 
    IAPPasteParam * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteParam_get_ID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParam_put_Label_Proxy( 
    IAPPasteParam * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IAPPasteParam_put_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParam_get_Label_Proxy( 
    IAPPasteParam * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteParam_get_Label_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParam_put_Value_Proxy( 
    IAPPasteParam * This,
    /* [in] */ BSTR Name);


void __RPC_STUB IAPPasteParam_put_Value_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParam_get_Value_Proxy( 
    IAPPasteParam * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IAPPasteParam_get_Value_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPasteParam_INTERFACE_DEFINED__ */


#ifndef __IAPPasteParams_INTERFACE_DEFINED__
#define __IAPPasteParams_INTERFACE_DEFINED__

/* interface IAPPasteParams */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPasteParams;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2536-74A1-4094-9939-73276031FCB4")
    IAPPasteParams : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Item( 
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPPasteParam **pParam) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Enum( 
            /* [retval][out] */ IUnknown **retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *pCount) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPasteParamsVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPasteParams * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPasteParams * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPasteParams * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPasteParams * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPasteParams * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPasteParams * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPasteParams * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Item )( 
            IAPPasteParams * This,
            /* [in] */ VARIANT index,
            /* [retval][out] */ IAPPasteParam **pParam);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Enum )( 
            IAPPasteParams * This,
            /* [retval][out] */ IUnknown **retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IAPPasteParams * This,
            /* [retval][out] */ long *pCount);
        
        END_INTERFACE
    } IAPPasteParamsVtbl;

    interface IAPPasteParams
    {
        CONST_VTBL struct IAPPasteParamsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPasteParams_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPasteParams_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPasteParams_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPasteParams_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPasteParams_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPasteParams_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPasteParams_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPasteParams_get_Item(This,index,pParam)	\
    (This)->lpVtbl -> get_Item(This,index,pParam)

#define IAPPasteParams_get_Enum(This,retval)	\
    (This)->lpVtbl -> get_Enum(This,retval)

#define IAPPasteParams_get_Count(This,pCount)	\
    (This)->lpVtbl -> get_Count(This,pCount)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParams_get_Item_Proxy( 
    IAPPasteParams * This,
    /* [in] */ VARIANT index,
    /* [retval][out] */ IAPPasteParam **pParam);


void __RPC_STUB IAPPasteParams_get_Item_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParams_get_Enum_Proxy( 
    IAPPasteParams * This,
    /* [retval][out] */ IUnknown **retval);


void __RPC_STUB IAPPasteParams_get_Enum_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteParams_get_Count_Proxy( 
    IAPPasteParams * This,
    /* [retval][out] */ long *pCount);


void __RPC_STUB IAPPasteParams_get_Count_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPasteParams_INTERFACE_DEFINED__ */


#ifndef __IAPPasteBuffer_INTERFACE_DEFINED__
#define __IAPPasteBuffer_INTERFACE_DEFINED__

/* interface IAPPasteBuffer */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IAPPasteBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("681C2537-74A1-4094-9939-73276031FCB4")
    IAPPasteBuffer : public IDispatch
    {
    public:
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Items( 
            /* [retval][out] */ IAPPasteItems **retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Params( 
            /* [retval][out] */ IAPPasteParams **retval) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Conflicts( 
            /* [retval][out] */ IAPConflicts **retval) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Context( 
            /* [in] */ VARIANT rhs) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Import( 
            /* [defaultvalue][optional][in] */ BSTR filename = L"") = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Export( 
            /* [defaultvalue][optional][in] */ BSTR filename = L"") = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Commit( 
            /* [optional][in] */ VARIANT force,
            /* [optional][in] */ VARIANT errorLog) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAPPasteBufferVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAPPasteBuffer * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAPPasteBuffer * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAPPasteBuffer * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IAPPasteBuffer * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IAPPasteBuffer * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IAPPasteBuffer * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IAPPasteBuffer * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Items )( 
            IAPPasteBuffer * This,
            /* [retval][out] */ IAPPasteItems **retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Params )( 
            IAPPasteBuffer * This,
            /* [retval][out] */ IAPPasteParams **retval);
        
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Conflicts )( 
            IAPPasteBuffer * This,
            /* [retval][out] */ IAPConflicts **retval);
        
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Context )( 
            IAPPasteBuffer * This,
            /* [in] */ VARIANT rhs);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Import )( 
            IAPPasteBuffer * This,
            /* [defaultvalue][optional][in] */ BSTR filename);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Export )( 
            IAPPasteBuffer * This,
            /* [defaultvalue][optional][in] */ BSTR filename);
        
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Commit )( 
            IAPPasteBuffer * This,
            /* [optional][in] */ VARIANT force,
            /* [optional][in] */ VARIANT errorLog);
        
        END_INTERFACE
    } IAPPasteBufferVtbl;

    interface IAPPasteBuffer
    {
        CONST_VTBL struct IAPPasteBufferVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAPPasteBuffer_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAPPasteBuffer_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IAPPasteBuffer_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IAPPasteBuffer_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IAPPasteBuffer_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IAPPasteBuffer_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IAPPasteBuffer_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IAPPasteBuffer_get_Items(This,retval)	\
    (This)->lpVtbl -> get_Items(This,retval)

#define IAPPasteBuffer_get_Params(This,retval)	\
    (This)->lpVtbl -> get_Params(This,retval)

#define IAPPasteBuffer_get_Conflicts(This,retval)	\
    (This)->lpVtbl -> get_Conflicts(This,retval)

#define IAPPasteBuffer_put_Context(This,rhs)	\
    (This)->lpVtbl -> put_Context(This,rhs)

#define IAPPasteBuffer_Import(This,filename)	\
    (This)->lpVtbl -> Import(This,filename)

#define IAPPasteBuffer_Export(This,filename)	\
    (This)->lpVtbl -> Export(This,filename)

#define IAPPasteBuffer_Commit(This,force,errorLog)	\
    (This)->lpVtbl -> Commit(This,force,errorLog)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_get_Items_Proxy( 
    IAPPasteBuffer * This,
    /* [retval][out] */ IAPPasteItems **retval);


void __RPC_STUB IAPPasteBuffer_get_Items_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_get_Params_Proxy( 
    IAPPasteBuffer * This,
    /* [retval][out] */ IAPPasteParams **retval);


void __RPC_STUB IAPPasteBuffer_get_Params_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propget][id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_get_Conflicts_Proxy( 
    IAPPasteBuffer * This,
    /* [retval][out] */ IAPConflicts **retval);


void __RPC_STUB IAPPasteBuffer_get_Conflicts_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [propput][id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_put_Context_Proxy( 
    IAPPasteBuffer * This,
    /* [in] */ VARIANT rhs);


void __RPC_STUB IAPPasteBuffer_put_Context_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_Import_Proxy( 
    IAPPasteBuffer * This,
    /* [defaultvalue][optional][in] */ BSTR filename);


void __RPC_STUB IAPPasteBuffer_Import_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_Export_Proxy( 
    IAPPasteBuffer * This,
    /* [defaultvalue][optional][in] */ BSTR filename);


void __RPC_STUB IAPPasteBuffer_Export_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [id] */ HRESULT STDMETHODCALLTYPE IAPPasteBuffer_Commit_Proxy( 
    IAPPasteBuffer * This,
    /* [optional][in] */ VARIANT force,
    /* [optional][in] */ VARIANT errorLog);


void __RPC_STUB IAPPasteBuffer_Commit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IAPPasteBuffer_INTERFACE_DEFINED__ */


#ifndef __IHNodeSelection_INTERFACE_DEFINED__
#define __IHNodeSelection_INTERFACE_DEFINED__

/* interface IHNodeSelection */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IHNodeSelection;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("D5DFF720-922A-40BD-8A2C-229913792D9C")
    IHNodeSelection : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Select( 
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ IHSelection **Selection) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectNodes( 
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ VARIANT *nodes) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectValues( 
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ VARIANT *values) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectAttributes( 
            /* [in] */ BSTR nodePath,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectLabelAttributes( 
            /* [in] */ BSTR nodePath,
            /* [in] */ long Dimension,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectSingleNodeAttributes( 
            /* [in] */ BSTR nodePath,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SelectAttributesOnNodes( 
            /* [in] */ VARIANT nodePathArray,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHNodeSelectionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHNodeSelection * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHNodeSelection * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHNodeSelection * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHNodeSelection * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHNodeSelection * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHNodeSelection * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHNodeSelection * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Select )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ IHSelection **Selection);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectNodes )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ VARIANT *nodes);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectValues )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [retval][out] */ VARIANT *values);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectAttributes )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectLabelAttributes )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [in] */ long Dimension,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectSingleNodeAttributes )( 
            IHNodeSelection * This,
            /* [in] */ BSTR nodePath,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SelectAttributesOnNodes )( 
            IHNodeSelection * This,
            /* [in] */ VARIANT nodePathArray,
            /* [in] */ VARIANT attributes,
            /* [retval][out] */ VARIANT *values);
        
        END_INTERFACE
    } IHNodeSelectionVtbl;

    interface IHNodeSelection
    {
        CONST_VTBL struct IHNodeSelectionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHNodeSelection_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHNodeSelection_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHNodeSelection_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHNodeSelection_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHNodeSelection_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHNodeSelection_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHNodeSelection_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHNodeSelection_Select(This,nodePath,Selection)	\
    (This)->lpVtbl -> Select(This,nodePath,Selection)

#define IHNodeSelection_SelectNodes(This,nodePath,nodes)	\
    (This)->lpVtbl -> SelectNodes(This,nodePath,nodes)

#define IHNodeSelection_SelectValues(This,nodePath,values)	\
    (This)->lpVtbl -> SelectValues(This,nodePath,values)

#define IHNodeSelection_SelectAttributes(This,nodePath,attributes,values)	\
    (This)->lpVtbl -> SelectAttributes(This,nodePath,attributes,values)

#define IHNodeSelection_SelectLabelAttributes(This,nodePath,Dimension,attributes,values)	\
    (This)->lpVtbl -> SelectLabelAttributes(This,nodePath,Dimension,attributes,values)

#define IHNodeSelection_SelectSingleNodeAttributes(This,nodePath,attributes,values)	\
    (This)->lpVtbl -> SelectSingleNodeAttributes(This,nodePath,attributes,values)

#define IHNodeSelection_SelectAttributesOnNodes(This,nodePathArray,attributes,values)	\
    (This)->lpVtbl -> SelectAttributesOnNodes(This,nodePathArray,attributes,values)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_Select_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [retval][out] */ IHSelection **Selection);


void __RPC_STUB IHNodeSelection_Select_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectNodes_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [retval][out] */ VARIANT *nodes);


void __RPC_STUB IHNodeSelection_SelectNodes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectValues_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [retval][out] */ VARIANT *values);


void __RPC_STUB IHNodeSelection_SelectValues_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectAttributes_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [in] */ VARIANT attributes,
    /* [retval][out] */ VARIANT *values);


void __RPC_STUB IHNodeSelection_SelectAttributes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectLabelAttributes_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [in] */ long Dimension,
    /* [in] */ VARIANT attributes,
    /* [retval][out] */ VARIANT *values);


void __RPC_STUB IHNodeSelection_SelectLabelAttributes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectSingleNodeAttributes_Proxy( 
    IHNodeSelection * This,
    /* [in] */ BSTR nodePath,
    /* [in] */ VARIANT attributes,
    /* [retval][out] */ VARIANT *values);


void __RPC_STUB IHNodeSelection_SelectSingleNodeAttributes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IHNodeSelection_SelectAttributesOnNodes_Proxy( 
    IHNodeSelection * This,
    /* [in] */ VARIANT nodePathArray,
    /* [in] */ VARIANT attributes,
    /* [retval][out] */ VARIANT *values);


void __RPC_STUB IHNodeSelection_SelectAttributesOnNodes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHNodeSelection_INTERFACE_DEFINED__ */


#ifndef __IHappConnectInfo_INTERFACE_DEFINED__
#define __IHappConnectInfo_INTERFACE_DEFINED__

/* interface IHappConnectInfo */
/* [object][oleautomation][helpstring][uuid] */ 


EXTERN_C const IID IID_IHappConnectInfo;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("35F04526-F10A-4D0D-9C6D-193E8CCD3A5F")
    IHappConnectInfo : public IDispatch
    {
    public:
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_Version( 
            /* [retval][out] */ BSTR *Version) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_filename( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_RunID( 
            /* [retval][out] */ BSTR *RunID) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_ClientDirectory( 
            /* [retval][out] */ BSTR *path) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_IsRemote( 
            /* [retval][out] */ VARIANT_BOOL *remote) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_username( 
            /* [retval][out] */ BSTR *user) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_Server( 
            /* [retval][out] */ BSTR *Server) = 0;
        
        virtual /* [helpstring][propget] */ HRESULT __stdcall get_ServerDirectory( 
            /* [retval][out] */ BSTR *path) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IHappConnectInfoVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IHappConnectInfo * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IHappConnectInfo * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IHappConnectInfo * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IHappConnectInfo * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IHappConnectInfo * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IHappConnectInfo * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IHappConnectInfo * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_Version )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *Version);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_filename )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *Name);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_RunID )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *RunID);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_ClientDirectory )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *path);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_IsRemote )( 
            IHappConnectInfo * This,
            /* [retval][out] */ VARIANT_BOOL *remote);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_username )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *user);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_Server )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *Server);
        
        /* [helpstring][propget] */ HRESULT ( __stdcall *get_ServerDirectory )( 
            IHappConnectInfo * This,
            /* [retval][out] */ BSTR *path);
        
        END_INTERFACE
    } IHappConnectInfoVtbl;

    interface IHappConnectInfo
    {
        CONST_VTBL struct IHappConnectInfoVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IHappConnectInfo_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IHappConnectInfo_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IHappConnectInfo_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IHappConnectInfo_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IHappConnectInfo_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IHappConnectInfo_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IHappConnectInfo_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IHappConnectInfo_get_Version(This,Version)	\
    (This)->lpVtbl -> get_Version(This,Version)

#define IHappConnectInfo_get_filename(This,Name)	\
    (This)->lpVtbl -> get_filename(This,Name)

#define IHappConnectInfo_get_RunID(This,RunID)	\
    (This)->lpVtbl -> get_RunID(This,RunID)

#define IHappConnectInfo_get_ClientDirectory(This,path)	\
    (This)->lpVtbl -> get_ClientDirectory(This,path)

#define IHappConnectInfo_get_IsRemote(This,remote)	\
    (This)->lpVtbl -> get_IsRemote(This,remote)

#define IHappConnectInfo_get_username(This,user)	\
    (This)->lpVtbl -> get_username(This,user)

#define IHappConnectInfo_get_Server(This,Server)	\
    (This)->lpVtbl -> get_Server(This,Server)

#define IHappConnectInfo_get_ServerDirectory(This,path)	\
    (This)->lpVtbl -> get_ServerDirectory(This,path)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_Version_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *Version);


void __RPC_STUB IHappConnectInfo_get_Version_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_filename_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *Name);


void __RPC_STUB IHappConnectInfo_get_filename_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_RunID_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *RunID);


void __RPC_STUB IHappConnectInfo_get_RunID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_ClientDirectory_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *path);


void __RPC_STUB IHappConnectInfo_get_ClientDirectory_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_IsRemote_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ VARIANT_BOOL *remote);


void __RPC_STUB IHappConnectInfo_get_IsRemote_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_username_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *user);


void __RPC_STUB IHappConnectInfo_get_username_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_Server_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *Server);


void __RPC_STUB IHappConnectInfo_get_Server_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][propget] */ HRESULT __stdcall IHappConnectInfo_get_ServerDirectory_Proxy( 
    IHappConnectInfo * This,
    /* [retval][out] */ BSTR *path);


void __RPC_STUB IHappConnectInfo_get_ServerDirectory_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IHappConnectInfo_INTERFACE_DEFINED__ */


EXTERN_C const CLSID CLSID_HappLS;

#ifdef __cplusplus

class DECLSPEC_UUID("7F3E8C3A-96FD-4D69-9700-35CF1CCF67AD")
HappLS;
#endif

EXTERN_C const CLSID CLSID_HappIP;

#ifdef __cplusplus

class DECLSPEC_UUID("0AB77320-8EA0-4239-B3CB-72C07BA3AC44")
HappIP;
#endif

EXTERN_C const CLSID CLSID_CHReadOnlyCompositeNode;

#ifdef __cplusplus

class DECLSPEC_UUID("E6CD1353-46DE-472E-8E32-874EA8ED08BC")
CHReadOnlyCompositeNode;
#endif

EXTERN_C const CLSID CLSID_CHReadWriteCompositeNode;

#ifdef __cplusplus

class DECLSPEC_UUID("7FE5B442-69EC-4B4E-BB54-73FCE4762F74")
CHReadWriteCompositeNode;
#endif

EXTERN_C const CLSID CLSID_CHPlotVal;

#ifdef __cplusplus

class DECLSPEC_UUID("3B963518-C826-4A23-8C07-FD6DE77764DC")
CHPlotVal;
#endif

EXTERN_C const CLSID CLSID_CHWizardPlot;

#ifdef __cplusplus

class DECLSPEC_UUID("A5760FEF-BE21-4B17-AEB9-22C15523DAE3")
CHWizardPlot;
#endif

EXTERN_C const CLSID CLSID_CHAdhocPlot;

#ifdef __cplusplus

class DECLSPEC_UUID("A92A2B32-5148-483A-BC04-000E36EB4BBC")
CHAdhocPlot;
#endif

EXTERN_C const CLSID CLSID_HappAProp;

#ifdef __cplusplus

class DECLSPEC_UUID("F9EA180A-0B39-4336-8328-C708EB1CEDE3")
HappAProp;
#endif

EXTERN_C const CLSID CLSID_HappAPropIP;

#ifdef __cplusplus

class DECLSPEC_UUID("F4D3B396-7650-4853-97E8-20ACE9A45CA9")
HappAPropIP;
#endif

EXTERN_C const CLSID CLSID_CHAdhocPlotAProp;

#ifdef __cplusplus

class DECLSPEC_UUID("653262B7-E643-4D4B-84B2-86C8E6A2EDB2")
CHAdhocPlotAProp;
#endif

EXTERN_C const CLSID CLSID_CHWizardPlotAProp;

#ifdef __cplusplus

class DECLSPEC_UUID("5764E7E4-600F-44C6-B439-60B09F5E4F7F")
CHWizardPlotAProp;
#endif
#endif /* __Happ_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


