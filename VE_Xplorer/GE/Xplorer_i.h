#include "VE_Open/skel/moduleS.h"

class Body_AMI_VEXplorerHandler_i
  : public virtual POA_Body::AMI_VEXplorerHandler
{
public:
  // Constructor 
  Body_AMI_VEXplorerHandler_i (void);
  
  // Destructor 
  virtual ~Body_AMI_VEXplorerHandler_i (void);
  
  virtual
  void GetStatusMessage (
      const char * ami_return_val
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void GetStatusMessage_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void SetParams (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetParams_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void SetID (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetID_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void GetID (
      ::CORBA::Long ami_return_val
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void GetID_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void SetName (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetName_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void GetName (
      const char * ami_return_val
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void GetName_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void SetNetwork (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetNetwork_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void SetCommand (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetCommand_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void RegisterUI (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void RegisterUI_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
  virtual
  void UnRegisterUI (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void UnRegisterUI_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
};