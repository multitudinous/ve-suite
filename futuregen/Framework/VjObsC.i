// -*- C++ -*-
//
// $Id$

// ****  Code generated by the The ACE ORB (TAO) IDL Compiler ****
// TAO and the TAO IDL Compiler have been developed by:
//       Center for Distributed Object Computing
//       Washington University
//       St. Louis, MO
//       USA
//       http://www.cs.wustl.edu/~schmidt/doc-center.html
// and
//       Distributed Object Computing Laboratory
//       University of California at Irvine
//       Irvine, CA
//       USA
//       http://doc.ece.uci.edu/
// and
//       Institute for Software Integrated Systems
//       Vanderbilt University
//       Nashville, TN
//       USA
//       http://www.isis.vanderbilt.edu/
//
// Information about TAO is available at:
//     http://www.cs.wustl.edu/~schmidt/TAO.html


// TAO_IDL - Generated from
// be/be_visitor_interface/interface_ci.cpp:70

#if !defined (_VJOBS___CI_)
#define _VJOBS___CI_

ACE_INLINE
VjObs::VjObs (
    TAO_Stub *objref,
    CORBA::Boolean _tao_collocated,
    TAO_Abstract_ServantBase *servant,
    TAO_ORB_Core *oc
  )
  : ACE_NESTED_CLASS (CORBA, Object) (
        objref,
        _tao_collocated,
        servant,
        oc
      ),
    the_TAO_VjObs_Proxy_Broker_ (0)
{
  this->VjObs_setup_collocation (_tao_collocated);
}

ACE_INLINE
VjObs::VjObs (
    IOP::IOR *ior,
    TAO_ORB_Core *oc
  )
  : ACE_NESTED_CLASS (CORBA, Object) (ior, oc),
    the_TAO_VjObs_Proxy_Broker_ (0)
{
}

#endif /* end #if !defined */

// TAO_IDL - Generated from
// be/be_visitor_interface/cdr_op_ci.cpp:72

 CORBA::Boolean operator<< (
    TAO_OutputCDR &,
    const VjObs_ptr
  );

 CORBA::Boolean operator>> (
    TAO_InputCDR &,
    VjObs_ptr &
  );

// TAO_IDL - Generated from
// be/be_visitor_sequence/cdr_op_ci.cpp:81

#if !defined _TAO_CDR_OP_VjObs_scalar_p_I_
#define _TAO_CDR_OP_VjObs_scalar_p_I_

CORBA::Boolean  operator<< (
    TAO_OutputCDR &,
    const VjObs::scalar_p &
  );

CORBA::Boolean  operator>> (
    TAO_InputCDR &,
    VjObs::scalar_p &
  );

#endif /* _TAO_CDR_OP_VjObs_scalar_p_I_ */

// TAO_IDL - Generated from
// be/be_visitor_sequence/cdr_op_ci.cpp:81

#if !defined _TAO_CDR_OP_VjObs_obj_p_I_
#define _TAO_CDR_OP_VjObs_obj_p_I_

CORBA::Boolean  operator<< (
    TAO_OutputCDR &,
    const VjObs::obj_p &
  );

CORBA::Boolean  operator>> (
    TAO_InputCDR &,
    VjObs::obj_p &
  );

#endif /* _TAO_CDR_OP_VjObs_obj_p_I_ */

// TAO_IDL - Generated from
// be/be_visitor_sequence/cdr_op_ci.cpp:81

#if !defined _TAO_CDR_OP_VjObs_obj_pd_I_
#define _TAO_CDR_OP_VjObs_obj_pd_I_

CORBA::Boolean  operator<< (
    TAO_OutputCDR &,
    const VjObs::obj_pd &
  );

CORBA::Boolean  operator>> (
    TAO_InputCDR &,
    VjObs::obj_pd &
  );

#endif /* _TAO_CDR_OP_VjObs_obj_pd_I_ */

