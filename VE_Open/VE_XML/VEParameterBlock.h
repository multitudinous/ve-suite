#ifndef _XML_VE_PARAMETER_BLOCK_H_
#define _XML_VE_PARAMETER_BLOCK_H_

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_XML/VEXMLObject.h"
namespace VE_XML{
   class VETransform;
   class VEDataValuePair;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML{
class VE_XML_EXPORTS VEParameterBlock : public VEXMLObject{
public:
   VEParameterBlock(DOMDocument* rootDoc,unsigned int id = 0);
   virtual ~VEParameterBlock();

   void SetId(unsigned int id);
   void SetTransform(VE_XML::VETransform* transform);
   void AddProperty(VE_XML::VEDataValuePair* prop);
   
   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   unsigned int GetId();
   VE_XML::VETransform* GetTransform();
   VE_XML::VEDataValuePair* GetProperty(std::string name);
   VE_XML::VEDataValuePair* GetProperty(unsigned int index);


protected:
	virtual void _updateVEElement( std::string );
   unsigned int _id;
   VE_XML::VETransform* _dcs;
   std::vector<VE_XML::VEDataValuePair*> _properties;
};
}
#endif// _XML_VE_PARAMETER_BLOCK_H_
