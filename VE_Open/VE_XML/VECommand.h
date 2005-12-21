#ifndef _XML_VE_COMMAND_H_
#define _XML_VE_COMMAND_H_

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

namespace VE_XML
{
   class VEDataValuePair;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS VECommand : public VEXMLObject
{
public:

   VECommand(DOMDocument* rootDoc);
   virtual ~VECommand();
   VECommand( const VECommand& );
   //equal operator
   VECommand& operator= ( const VECommand& );
   
   //The name of the command to execute
   void SetCommandName( std::string name );

   void AddDataValuePair(VE_XML::VEDataValuePair* commandValuePair);

   void ExtractCmdNameFromElement(DOMElement* commandElement);

   
   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   std::string GetCommandName();

   VE_XML::VEDataValuePair* GetDataValuePair(std::string dataValueName);
   VE_XML::VEDataValuePair* GetDataValuePair(unsigned int index);

   unsigned int GetNumberOfDataValuePairs();

protected:
   void _updateVEElement( std::string );
   void _updateCommandName( void );
   void _updateDataValuePairs( void );
   unsigned int _nDataValuePairs;
   std::string _cmdName;
   std::vector< VE_XML::VEDataValuePair* > _dataValuePairs;  
};
}
#endif// _XML_VE_COMMAND_H_
