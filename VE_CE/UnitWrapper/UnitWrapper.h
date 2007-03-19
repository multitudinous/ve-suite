// TODO: Fill in documentation, document the virtual functions.
#ifndef UNIT_WRAPPER_H_
#define UNIT_WRAPPER_H_

#include <VE_Open/skel/moduleS.h>

#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/Model/Model.h>

#include <vector>

namespace VE_CE
{
   class EventHandler;
}

///??
class  UnitWrapper : public virtual POA_Body::Unit
{
public:
   ///Constructor 
   UnitWrapper( Body::Executive_ptr exec, std::string name );
   ///Default constructor
   UnitWrapper(){;}
   ///Destructor 
   virtual ~UnitWrapper( void );

protected:
   Body::Executive_var executive_;
   unsigned int return_state;
   ///??
   std::string UnitName_;
   //::Types::ArrayLong_var id_;
   ///??
   CORBA::Long activeId;
   ///??
   std::string status_;
   ///??
   std::string data_;
   ///??
   std::map< std::string, VE_XML::VE_Model::Model* > xmlModelMap;
   ///??
   std::map< std::string, std::vector< VE_XML::XMLObject* > > inputsMap;
   ///??
   std::map< std::string, std::vector< VE_XML::XMLObject* > > resultsMap;
   ///??
   std::map< std::string, VE_CE::EventHandler* > eventHandlerMap;
   
public:

   virtual
   void StartCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   void StopCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   void PauseCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   void Resume (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   char * GetStatusMessage (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   char * GetUserData (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   void SetParams (
                   ::CORBA::Long module_id,
      const char * param
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   void SetID (
      ::CORBA::Long id
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
      void SetCurID (
                     ::CORBA::Long id
                     )
      ACE_THROW_SPEC ((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                       ));
  
   virtual
      ::Types::ArrayLong * GetID (
                                  
                                  )
      ACE_THROW_SPEC ((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                       ));

   virtual
      ::CORBA::Long GetCurID (
                              
                              )
      ACE_THROW_SPEC ((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
 
                  ));
   virtual
   void SetName (
      const char * name
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   char * GetName (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
   char * Query ( const char * command
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

   virtual
      void DeleteModuleInstance (
                                 ::CORBA::Long module_id
                                 )
      ACE_THROW_SPEC ((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                       ));
};


#endif /* UNIT_WRAPPER_H_  */

