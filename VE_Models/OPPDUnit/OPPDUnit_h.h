

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 6.00.0361 */
/* at Sun Aug 15 12:41:00 2004
 */
/* Compiler settings for .\OPPDUnit.idl:
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


#ifndef __OPPDUnit_h_h__
#define __OPPDUnit_h_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __IOPPDUnit_FWD_DEFINED__
#define __IOPPDUnit_FWD_DEFINED__
typedef interface IOPPDUnit IOPPDUnit;
#endif 	/* __IOPPDUnit_FWD_DEFINED__ */


#ifndef __OPPDUnit_FWD_DEFINED__
#define __OPPDUnit_FWD_DEFINED__

#ifdef __cplusplus
typedef class OPPDUnit OPPDUnit;
#else
typedef struct OPPDUnit OPPDUnit;
#endif /* __cplusplus */

#endif 	/* __OPPDUnit_FWD_DEFINED__ */


#ifdef __cplusplus
extern "C"{
#endif 

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void * ); 


#ifndef __OPPDUnit_LIBRARY_DEFINED__
#define __OPPDUnit_LIBRARY_DEFINED__

/* library OPPDUnit */
/* [version][uuid] */ 


EXTERN_C const IID LIBID_OPPDUnit;

#ifndef __IOPPDUnit_DISPINTERFACE_DEFINED__
#define __IOPPDUnit_DISPINTERFACE_DEFINED__

/* dispinterface IOPPDUnit */
/* [uuid] */ 


EXTERN_C const IID DIID_IOPPDUnit;

#if defined(__cplusplus) && !defined(CINTERFACE)

    MIDL_INTERFACE("395E090F-393C-4AF9-91CA-8EC8EAF660C0")
    IOPPDUnit : public IDispatch
    {
    };
    
#else 	/* C style interface */

    typedef struct IOPPDUnitVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IOPPDUnit * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IOPPDUnit * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IOPPDUnit * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IOPPDUnit * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IOPPDUnit * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IOPPDUnit * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IOPPDUnit * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } IOPPDUnitVtbl;

    interface IOPPDUnit
    {
        CONST_VTBL struct IOPPDUnitVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IOPPDUnit_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IOPPDUnit_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IOPPDUnit_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IOPPDUnit_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IOPPDUnit_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IOPPDUnit_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IOPPDUnit_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)

#endif /* COBJMACROS */


#endif 	/* C style interface */


#endif 	/* __IOPPDUnit_DISPINTERFACE_DEFINED__ */


EXTERN_C const CLSID CLSID_OPPDUnit;

#ifdef __cplusplus

class DECLSPEC_UUID("458DEF56-B4F7-4F57-838E-7E40C7D9C6BE")
OPPDUnit;
#endif
#endif /* __OPPDUnit_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


