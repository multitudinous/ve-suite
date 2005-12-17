#ifndef _XML_VE_USER_H_
#define _XML_VE_USER_H_

namespace VE_XML{
   class VEStateInfo;
}

#include <xercesc/dom/DOM.hpp>

#include <iostream>

#include "VE_Installer/include/VEConfig.h"
#include "VE_XML/VEXMLObject.h"
namespace VE_XML{
class VE_XML_EXPORTS VEUser : public VEXMLObject{
public:
   VEUser(DOMDocument* rootDoc);
   virtual ~VEUser();
   //Valid status
   //MASTER == controlling changes of the main graphics state
   //SLAVE == observing changes of the main graphics state
   typedef std::string VEControlStatus;
   
   void SetUserId(std::string id);
   void SetControlStatus(VEControlStatus cs);
   void SetStateInfo(VE_XML::VEStateInfo* userState);

   std::string GetUserId();
   VEControlStatus GetControlStatus();
   VE_XML::VEStateInfo* GetUserStateInfo();

  virtual void SetObjectFromXMLData(DOMNode* xmlInput);   

protected:
   virtual void _updateVEElement( std::string );
   std::string _userId;
   VE_XML::VEStateInfo* _stateInfo;
   VEControlStatus _controlStatus;
};
}
#endif// _XML_VE_USER_H_
