// This file is generated by omniidl (C++ backend)- omniORB_4_0. Do not edit.

#include "VjDesignObs.h"
#include <omniORB4/IOP_S.h>
#include <omniORB4/IOP_C.h>
#include <omniORB4/callDescriptor.h>
#include <omniORB4/callHandle.h>
#include <omniORB4/objTracker.h>


OMNI_USING_NAMESPACE(omni)

static const char* _0RL_library_version = omniORB_4_0;



VjDesignObs_ptr VjDesignObs_Helper::_nil() {
  return ::VjDesignObs::_nil();
}

CORBA::Boolean VjDesignObs_Helper::is_nil(::VjDesignObs_ptr p) {
  return CORBA::is_nil(p);

}

void VjDesignObs_Helper::release(::VjDesignObs_ptr p) {
  CORBA::release(p);
}

void VjDesignObs_Helper::duplicate(::VjDesignObs_ptr p) {
  if( p && !p->_NP_is_nil() )  omni::duplicateObjRef(p);
}

void VjDesignObs_Helper::marshalObjRef(::VjDesignObs_ptr obj, cdrStream& s) {
  ::VjDesignObs::_marshalObjRef(obj, s);
}

VjDesignObs_ptr VjDesignObs_Helper::unmarshalObjRef(cdrStream& s) {
  return ::VjDesignObs::_unmarshalObjRef(s);
}

VjDesignObs_ptr
VjDesignObs::_duplicate(::VjDesignObs_ptr obj)
{
  if( obj && !obj->_NP_is_nil() )  omni::duplicateObjRef(obj);

  return obj;
}


VjDesignObs_ptr
VjDesignObs::_narrow(CORBA::Object_ptr obj)
{
  if( !obj || obj->_NP_is_nil() || obj->_NP_is_pseudo() ) return _nil();
  _ptr_type e = (_ptr_type) obj->_PR_getobj()->_realNarrow(_PD_repoId);
  return e ? e : _nil();
}


VjDesignObs_ptr
VjDesignObs::_unchecked_narrow(CORBA::Object_ptr obj)
{
  if( !obj || obj->_NP_is_nil() || obj->_NP_is_pseudo() ) return _nil();
  _ptr_type e = (_ptr_type) obj->_PR_getobj()->_uncheckedNarrow(_PD_repoId);
  return e ? e : _nil();
}


VjDesignObs_ptr
VjDesignObs::_nil()
{
  static _objref_VjDesignObs* _the_nil_ptr = 0;
  if( !_the_nil_ptr ) {
    omni::nilRefLock().lock();
    if( !_the_nil_ptr ) {
      _the_nil_ptr = new _objref_VjDesignObs;
      registerNilCorbaObject(_the_nil_ptr);
    }
    omni::nilRefLock().unlock();
  }
  return _the_nil_ptr;
}

const char* VjDesignObs::_PD_repoId = "IDL:VjDesignObs:1.0";


_objref_VjDesignObs::~_objref_VjDesignObs() {}


_objref_VjDesignObs::_objref_VjDesignObs(omniIOR* ior, omniIdentity* id) :
   omniObjRef(::VjDesignObs::_PD_repoId, ior, id, 1)
   
   
{
  _PR_setobj(this);
}

void*
_objref_VjDesignObs::_ptrToObjRef(const char* id)
{
  if( id == ::VjDesignObs::_PD_repoId )
    return (::VjDesignObs_ptr) this;
  
  if( id == CORBA::Object::_PD_repoId )
    return (CORBA::Object_ptr) this;

  if( omni::strMatch(id, ::VjDesignObs::_PD_repoId) )
    return (::VjDesignObs_ptr) this;
  
  if( omni::strMatch(id, CORBA::Object::_PD_repoId) )
    return (CORBA::Object_ptr) this;

  return 0;
}

// Proxy call descriptor class. Mangled signature:
//  void
class _0RL_cd_06C7BF6C189A5E24_00000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_00000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  
    
  
  
};

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_10000000(omniCallDescriptor* cd, omniServant* svnt)
{
  
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->update();


}

void _objref_VjDesignObs::update()
{
  _0RL_cd_06C7BF6C189A5E24_00000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_10000000, "update", 7);


  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  void_i_cshort
class _0RL_cd_06C7BF6C189A5E24_20000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_20000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  void marshalArguments(cdrStream&);
  void unmarshalArguments(cdrStream&);

    
  
  CORBA::Short arg_0;
};

void _0RL_cd_06C7BF6C189A5E24_20000000::marshalArguments(cdrStream& _n)
{
  arg_0 >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_20000000::unmarshalArguments(cdrStream& _n)
{
  (CORBA::Short&)arg_0 <<= _n;

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_30000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setClientInfoFlag(tcd->arg_0);


}

void _objref_VjDesignObs::setClientInfoFlag(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_30000000, "setClientInfoFlag", 18);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  _cshort
class _0RL_cd_06C7BF6C189A5E24_40000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_40000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  
  void unmarshalReturnedValues(cdrStream&);
  void marshalReturnedValues(cdrStream&);
  
  
  CORBA::Short result;
};

void _0RL_cd_06C7BF6C189A5E24_40000000::marshalReturnedValues(cdrStream& _n)
{
  result >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_40000000::unmarshalReturnedValues(cdrStream& _n)
{
  (CORBA::Short&)result <<= _n;

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_50000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getClientInfoFlag();


}

CORBA::Short _objref_VjDesignObs::getClientInfoFlag()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_50000000, "getClientInfoFlag", 18);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_60000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setInteractiveState_UI(tcd->arg_0);


}

void _objref_VjDesignObs::setInteractiveState_UI(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_60000000, "setInteractiveState_UI", 23);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_70000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getInteractiveState_UI();


}

CORBA::Short _objref_VjDesignObs::getInteractiveState_UI()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_70000000, "getInteractiveState_UI", 23);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_80000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setInteractiveState_cfd(tcd->arg_0);


}

void _objref_VjDesignObs::setInteractiveState_cfd(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_80000000, "setInteractiveState_cfd", 24);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_90000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getInteractiveState_cfd();


}

CORBA::Short _objref_VjDesignObs::getInteractiveState_cfd()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_90000000, "getInteractiveState_cfd", 24);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_a0000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setInteractiveGA(tcd->arg_0);


}

void _objref_VjDesignObs::setInteractiveGA(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_a0000000, "setInteractiveGA", 17);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_b0000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getInteractiveGA();


}

CORBA::Short _objref_VjDesignObs::getInteractiveGA()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_b0000000, "getInteractiveGA", 17);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Proxy call descriptor class. Mangled signature:
//  void_i_cVjDesignObs_mdesign__params
class _0RL_cd_06C7BF6C189A5E24_c0000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_c0000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  void marshalArguments(cdrStream&);
  void unmarshalArguments(cdrStream&);

    
  
  VjDesignObs::design_params_var arg_0_;
  const VjDesignObs::design_params* arg_0;
};

void _0RL_cd_06C7BF6C189A5E24_c0000000::marshalArguments(cdrStream& _n)
{
  (const VjDesignObs::design_params&) *arg_0 >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_c0000000::unmarshalArguments(cdrStream& _n)
{
  arg_0_ = new VjDesignObs::design_params;
  (VjDesignObs::design_params&)arg_0_ <<= _n;
  arg_0 = &arg_0_.in();

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_d0000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000* tcd = (_0RL_cd_06C7BF6C189A5E24_c0000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setDesignParams(*tcd->arg_0);


}

void _objref_VjDesignObs::setDesignParams(const VjDesignObs::design_params& value)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_d0000000, "setDesignParams", 16);
  _call_desc.arg_0 = &(VjDesignObs::design_params&) value;

  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  void_i_cVjDesignObs_mactive__flag
class _0RL_cd_06C7BF6C189A5E24_e0000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_e0000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  void marshalArguments(cdrStream&);
  void unmarshalArguments(cdrStream&);

    
  
  VjDesignObs::active_flag_var arg_0_;
  const VjDesignObs::active_flag* arg_0;
};

void _0RL_cd_06C7BF6C189A5E24_e0000000::marshalArguments(cdrStream& _n)
{
  (const VjDesignObs::active_flag&) *arg_0 >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_e0000000::unmarshalArguments(cdrStream& _n)
{
  arg_0_ = new VjDesignObs::active_flag;
  (VjDesignObs::active_flag&)arg_0_ <<= _n;
  arg_0 = &arg_0_.in();

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_f0000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_e0000000* tcd = (_0RL_cd_06C7BF6C189A5E24_e0000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setActiveDesignParams(*tcd->arg_0);


}

void _objref_VjDesignObs::setActiveDesignParams(const VjDesignObs::active_flag& value)
{
  _0RL_cd_06C7BF6C189A5E24_e0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_f0000000, "setActiveDesignParams", 22);
  _call_desc.arg_0 = &(VjDesignObs::active_flag&) value;

  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  void_i_cstring
class _0RL_cd_06C7BF6C189A5E24_01000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_01000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  void marshalArguments(cdrStream&);
  void unmarshalArguments(cdrStream&);

    
  
  CORBA::String_var arg_0_;
  const char* arg_0;
};

void _0RL_cd_06C7BF6C189A5E24_01000000::marshalArguments(cdrStream& _n)
{
  _n.marshalString(arg_0,0);

}

void _0RL_cd_06C7BF6C189A5E24_01000000::unmarshalArguments(cdrStream& _n)
{
  arg_0_ = _n.unmarshalString(0);
  arg_0 = arg_0_.in();

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_11000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_01000000* tcd = (_0RL_cd_06C7BF6C189A5E24_01000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setVRserver_name(tcd->arg_0);


}

void _objref_VjDesignObs::setVRserver_name(const char* value)
{
  _0RL_cd_06C7BF6C189A5E24_01000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_11000000, "setVRserver_name", 17);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  _cstring
class _0RL_cd_06C7BF6C189A5E24_21000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_21000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  
  void unmarshalReturnedValues(cdrStream&);
  void marshalReturnedValues(cdrStream&);
  
  
  CORBA::String_var result;
};

void _0RL_cd_06C7BF6C189A5E24_21000000::marshalReturnedValues(cdrStream& _n)
{
  _n.marshalString(result,0);

}

void _0RL_cd_06C7BF6C189A5E24_21000000::unmarshalReturnedValues(cdrStream& _n)
{
  result = _n.unmarshalString(0);

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_31000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_21000000* tcd = (_0RL_cd_06C7BF6C189A5E24_21000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getVRserver_name();


}

char* _objref_VjDesignObs::getVRserver_name()
{
  _0RL_cd_06C7BF6C189A5E24_21000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_31000000, "getVRserver_name", 17);


  _invoke(_call_desc);
  return _call_desc.result._retn();


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_41000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setMode_oldModel(tcd->arg_0);


}

void _objref_VjDesignObs::setMode_oldModel(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_41000000, "setMode_oldModel", 17);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_51000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getMode_oldModel();


}

CORBA::Short _objref_VjDesignObs::getMode_oldModel()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_51000000, "getMode_oldModel", 17);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_61000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000* tcd = (_0RL_cd_06C7BF6C189A5E24_c0000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setTrans_oldModel(*tcd->arg_0);


}

void _objref_VjDesignObs::setTrans_oldModel(const VjDesignObs::design_params& value)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_61000000, "setTrans_oldModel", 18);
  _call_desc.arg_0 = &(VjDesignObs::design_params&) value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_71000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000* tcd = (_0RL_cd_06C7BF6C189A5E24_c0000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setRot_oldModel(*tcd->arg_0);


}

void _objref_VjDesignObs::setRot_oldModel(const VjDesignObs::design_params& value)
{
  _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_71000000, "setRot_oldModel", 16);
  _call_desc.arg_0 = &(VjDesignObs::design_params&) value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_81000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_01000000* tcd = (_0RL_cd_06C7BF6C189A5E24_01000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setNewVtkFileName(tcd->arg_0);


}

void _objref_VjDesignObs::setNewVtkFileName(const char* value)
{
  _0RL_cd_06C7BF6C189A5E24_01000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_81000000, "setNewVtkFileName", 18);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_91000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_21000000* tcd = (_0RL_cd_06C7BF6C189A5E24_21000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getNewVtkFileName();


}

char* _objref_VjDesignObs::getNewVtkFileName()
{
  _0RL_cd_06C7BF6C189A5E24_21000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_91000000, "getNewVtkFileName", 18);


  _invoke(_call_desc);
  return _call_desc.result._retn();


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_a1000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_20000000* tcd = (_0RL_cd_06C7BF6C189A5E24_20000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setCheckedGene(tcd->arg_0);


}

void _objref_VjDesignObs::setCheckedGene(CORBA::Short value)
{
  _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_a1000000, "setCheckedGene", 15);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_b1000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_40000000* tcd = (_0RL_cd_06C7BF6C189A5E24_40000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getCheckedGene();


}

CORBA::Short _objref_VjDesignObs::getCheckedGene()
{
  _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_b1000000, "getCheckedGene", 15);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Proxy call descriptor class. Mangled signature:
//  void_i_cdouble
class _0RL_cd_06C7BF6C189A5E24_c1000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_c1000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  void marshalArguments(cdrStream&);
  void unmarshalArguments(cdrStream&);

    
  
  CORBA::Double arg_0;
};

void _0RL_cd_06C7BF6C189A5E24_c1000000::marshalArguments(cdrStream& _n)
{
  arg_0 >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_c1000000::unmarshalArguments(cdrStream& _n)
{
  (CORBA::Double&)arg_0 <<= _n;

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_d1000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_c1000000* tcd = (_0RL_cd_06C7BF6C189A5E24_c1000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setCrossoverRate(tcd->arg_0);


}

void _objref_VjDesignObs::setCrossoverRate(CORBA::Double value)
{
  _0RL_cd_06C7BF6C189A5E24_c1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_d1000000, "setCrossoverRate", 17);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Proxy call descriptor class. Mangled signature:
//  _cdouble
class _0RL_cd_06C7BF6C189A5E24_e1000000
  : public omniCallDescriptor
{
public:
  inline _0RL_cd_06C7BF6C189A5E24_e1000000(LocalCallFn lcfn,const char* op_,size_t oplen,_CORBA_Boolean upcall=0):
     omniCallDescriptor(lcfn, op_, oplen, 0, 0, 0, upcall) {}
  
  
  void unmarshalReturnedValues(cdrStream&);
  void marshalReturnedValues(cdrStream&);
  
  
  CORBA::Double result;
};

void _0RL_cd_06C7BF6C189A5E24_e1000000::marshalReturnedValues(cdrStream& _n)
{
  result >>= _n;

}

void _0RL_cd_06C7BF6C189A5E24_e1000000::unmarshalReturnedValues(cdrStream& _n)
{
  (CORBA::Double&)result <<= _n;

}

// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_f1000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_e1000000* tcd = (_0RL_cd_06C7BF6C189A5E24_e1000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getCrossoverRate();


}

CORBA::Double _objref_VjDesignObs::getCrossoverRate()
{
  _0RL_cd_06C7BF6C189A5E24_e1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_f1000000, "getCrossoverRate", 17);


  _invoke(_call_desc);
  return _call_desc.result;


}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_02000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_c1000000* tcd = (_0RL_cd_06C7BF6C189A5E24_c1000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  impl->setMutationRate(tcd->arg_0);


}

void _objref_VjDesignObs::setMutationRate(CORBA::Double value)
{
  _0RL_cd_06C7BF6C189A5E24_c1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_02000000, "setMutationRate", 16);
  _call_desc.arg_0 = value;

  _invoke(_call_desc);



}
// Local call call-back function.
static void
_0RL_lcfn_06C7BF6C189A5E24_12000000(omniCallDescriptor* cd, omniServant* svnt)
{
  _0RL_cd_06C7BF6C189A5E24_e1000000* tcd = (_0RL_cd_06C7BF6C189A5E24_e1000000*)cd;
  _impl_VjDesignObs* impl = (_impl_VjDesignObs*) svnt->_ptrToInterface(VjDesignObs::_PD_repoId);
  tcd->result = impl->getMutationRate();


}

CORBA::Double _objref_VjDesignObs::getMutationRate()
{
  _0RL_cd_06C7BF6C189A5E24_e1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_12000000, "getMutationRate", 16);


  _invoke(_call_desc);
  return _call_desc.result;


}
_pof_VjDesignObs::~_pof_VjDesignObs() {}


omniObjRef*
_pof_VjDesignObs::newObjRef(omniIOR* ior, omniIdentity* id)
{
  return new ::_objref_VjDesignObs(ior, id);
}


CORBA::Boolean
_pof_VjDesignObs::is_a(const char* id) const
{
  if( omni::ptrStrMatch(id, ::VjDesignObs::_PD_repoId) )
    return 1;
  
  return 0;
}

const _pof_VjDesignObs _the_pof_VjDesignObs;

_impl_VjDesignObs::~_impl_VjDesignObs() {}


CORBA::Boolean
_impl_VjDesignObs::_dispatch(omniCallHandle& _handle)
{
  const char* op = _handle.operation_name();

  if( omni::strMatch(op, "update") ) {

    _0RL_cd_06C7BF6C189A5E24_00000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_10000000, "update", 7, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setClientInfoFlag") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_30000000, "setClientInfoFlag", 18, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getClientInfoFlag") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_50000000, "getClientInfoFlag", 18, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setInteractiveState_UI") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_60000000, "setInteractiveState_UI", 23, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getInteractiveState_UI") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_70000000, "getInteractiveState_UI", 23, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setInteractiveState_cfd") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_80000000, "setInteractiveState_cfd", 24, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getInteractiveState_cfd") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_90000000, "getInteractiveState_cfd", 24, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setInteractiveGA") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_a0000000, "setInteractiveGA", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getInteractiveGA") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_b0000000, "getInteractiveGA", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setDesignParams") ) {

    _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_d0000000, "setDesignParams", 16, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setActiveDesignParams") ) {

    _0RL_cd_06C7BF6C189A5E24_e0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_f0000000, "setActiveDesignParams", 22, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setVRserver_name") ) {

    _0RL_cd_06C7BF6C189A5E24_01000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_11000000, "setVRserver_name", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getVRserver_name") ) {

    _0RL_cd_06C7BF6C189A5E24_21000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_31000000, "getVRserver_name", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setMode_oldModel") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_41000000, "setMode_oldModel", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getMode_oldModel") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_51000000, "getMode_oldModel", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setTrans_oldModel") ) {

    _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_61000000, "setTrans_oldModel", 18, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setRot_oldModel") ) {

    _0RL_cd_06C7BF6C189A5E24_c0000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_71000000, "setRot_oldModel", 16, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setNewVtkFileName") ) {

    _0RL_cd_06C7BF6C189A5E24_01000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_81000000, "setNewVtkFileName", 18, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getNewVtkFileName") ) {

    _0RL_cd_06C7BF6C189A5E24_21000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_91000000, "getNewVtkFileName", 18, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setCheckedGene") ) {

    _0RL_cd_06C7BF6C189A5E24_20000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_a1000000, "setCheckedGene", 15, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getCheckedGene") ) {

    _0RL_cd_06C7BF6C189A5E24_40000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_b1000000, "getCheckedGene", 15, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setCrossoverRate") ) {

    _0RL_cd_06C7BF6C189A5E24_c1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_d1000000, "setCrossoverRate", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getCrossoverRate") ) {

    _0RL_cd_06C7BF6C189A5E24_e1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_f1000000, "getCrossoverRate", 17, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "setMutationRate") ) {

    _0RL_cd_06C7BF6C189A5E24_c1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_02000000, "setMutationRate", 16, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }

  if( omni::strMatch(op, "getMutationRate") ) {

    _0RL_cd_06C7BF6C189A5E24_e1000000 _call_desc(_0RL_lcfn_06C7BF6C189A5E24_12000000, "getMutationRate", 16, 1);
    
    _handle.upcall(this,_call_desc);
    return 1;
  }


  return 0;
}

void*
_impl_VjDesignObs::_ptrToInterface(const char* id)
{
  if( id == ::VjDesignObs::_PD_repoId )
    return (_impl_VjDesignObs*) this;
  
  if( id == CORBA::Object::_PD_repoId )
    return (void*) 1;

  if( omni::strMatch(id, ::VjDesignObs::_PD_repoId) )
    return (_impl_VjDesignObs*) this;
  
  if( omni::strMatch(id, CORBA::Object::_PD_repoId) )
    return (void*) 1;
  return 0;
}

const char*
_impl_VjDesignObs::_mostDerivedRepoId()
{
  return ::VjDesignObs::_PD_repoId;
}

POA_VjDesignObs::~POA_VjDesignObs() {}

