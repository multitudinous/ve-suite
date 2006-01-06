#include <iostream>
#include "VE_Open/VE_XML/VE_CAD/CADXMLReaderWriter.h"
#include "VE_Open/VE_XML/VE_CAD/CADNode.h"
#include "VE_Open/VE_XML/VE_CAD/CADPart.h"
#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////
//Constructor               //
//////////////////////////////
CADXMLReaderWriter::CADXMLReaderWriter()
:VE_XML::XMLReaderWriter()
{
   _rootNode = 0;
}
//////////////////////////////////////////////////
CADXMLReaderWriter::CADXMLReaderWriter(const CADXMLReaderWriter& fr)
:VE_XML::XMLReaderWriter(fr)
{
   _rootNode = new VE_CAD::CADNode(*fr._rootNode);
}
/////////////////////////////
CADXMLReaderWriter::~CADXMLReaderWriter()
{
  
   if(_rootNode)
   {
      delete _rootNode;
      _rootNode = 0;
   }
}
////////////////////////////////////////////
VE_CAD::CADNode* CADXMLReaderWriter::GetRootNode()
{
   return _rootNode;
}
////////////////////////////////////////////////////////////////////////////
void CADXMLReaderWriter::_populateStructureFromDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument)
{

   //Get the first element and check it's type
   DOMNodeList* assemblies = rootDocument->getElementsByTagName( xercesString("CADAssembly") );
   
   unsigned int nAssemblies = assemblies->getLength();
   if(nAssemblies)
   {
      if(_rootNode)
      {
         if(_rootNode->GetNodeType() != std::string("Assembly"))
         {
            delete _rootNode;
            _rootNode = 0;
            _rootNode = new VE_CAD::CADAssembly(rootDocument);
         }else{
            _rootNode->SetOwnerDocument(rootDocument);
         }
      }else{
         _rootNode = new VE_CAD::CADAssembly(rootDocument);
      }
      _rootNode->SetObjectFromXMLData(assemblies->item(0));
   }
   else
   {
      ///check for single part
      DOMNodeList* parts = rootDocument->getElementsByTagName( xercesString("CADPart") );
      unsigned int nParts = parts->getLength();
      if(nParts)
      {
         
         //probably should create an assembly and then add all the parts but
         //assuming there is only one part file for now
         if(_rootNode)
         {
            if(_rootNode->GetNodeType() != std::string("Part"))
            {
               delete _rootNode;
               _rootNode = 0;
               _rootNode = new VE_CAD::CADPart(rootDocument);
            }else{
              _rootNode->SetOwnerDocument(rootDocument);
            }
         }else{
            _rootNode = new VE_CAD::CADPart(rootDocument);
         }
         _rootNode->SetObjectFromXMLData(parts->item(0)); 
      }
   }
}
//////////////////////////////////////////////////////////////
CADXMLReaderWriter& CADXMLReaderWriter::operator=(const CADXMLReaderWriter& rhs)
{
   if(&rhs != this){
      XMLReaderWriter::operator=(rhs);
      if(_rootNode)
      {
         delete _rootNode;
	 _rootNode = 0;
      }
      _rootNode = new VE_CAD::CADNode(*rhs._rootNode);
   }
   return *this;
}
