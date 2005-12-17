#ifndef _XML_VE_STATE_INFO_H_
#define _XML_VE_STATE_INFO_H_
#include <vector>
#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
namespace VE_XML
{
   class VECommand;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML{
class VE_XML_EXPORTS VEStateInfo : public VEXMLObject{
public:
   VEStateInfo(DOMDocument* rootDoc);
   virtual ~VEStateInfo();

   void AddState(VE_XML::VECommand* state);
   void ClearState();
   
   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);

   VE_XML::VECommand* GetState(std::string name);
   VE_XML::VECommand* GetState(unsigned int index);

protected:
	virtual void _updateVEElement( std::string );
   void _updateCommands();
   std::vector<VE_XML::VECommand*> _stateInfo;
};
}
#endif// _XML_VE_STATE_INFO_H_
