#include <iostream>
#include "VE_Open/VE_XML/VE_CAD/CADXMLReader.h"
#include "VE_Open/VE_XML/VE_CAD/CADNode.h"
#include "VE_Open/VE_XML/VE_CAD/CADPart.h"
#include "VE_Open/VE_XML/VE_CAD/CADAssembly.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////
//Constructor               //
//////////////////////////////
CADXMLReader::CADXMLReader()
:VE_XML::XMLReader()
{
   _rootNode = 0;
}
//////////////////////////////////////////////////
CADXMLReader::CADXMLReader(const CADXMLReader& fr)
:VE_XML::XMLReader(fr)
{
   _rootNode = new VE_CAD::CADNode(*fr._rootNode);
}
/////////////////////////////
CADXMLReader::~CADXMLReader()
{
  
   if(_rootNode)
   {
      delete _rootNode;
      _rootNode = 0;
   }
}
////////////////////////////////////////////
VE_CAD::CADNode* CADXMLReader::GetRootNode()
{
   return _rootNode;
}
////////////////////////////////////////////////////////////////////////////
void CADXMLReader::_populateStructureFromDocument(xercesc_2_6::DOMDocument* rootDocument)
{

   //Get the first element and check it's type
   DOMNodeList* assemblies = rootDocument->getElementsByTagName( xercesString("CADAssembly") );
   
   unsigned int nAssemblies = assemblies->getLength();
   if(nAssemblies)
   {
      _rootNode = new VE_CAD::CADAssembly(rootDocument);
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
         ///assuming there is only one part file for now
         _rootNode = new VE_CAD::CADPart(rootDocument);
         _rootNode->SetObjectFromXMLData(parts->item(0)); 
      }
   }
}
//////////////////////////////////////////////////////////////
CADXMLReader& CADXMLReader::operator=(const CADXMLReader& rhs)
{
   if(&rhs != this){
      XMLReader::operator=(rhs);
      if(_rootNode)
      {
         delete _rootNode;
	 _rootNode = 0;
      }
      _rootNode = new VE_CAD::CADNode(*rhs._rootNode);
   }
   return *this;
}
