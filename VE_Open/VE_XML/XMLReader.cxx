#include "VE_Open/VE_XML/XMLReader.h"
#include "VE_Conductor/Framework/DOMDocumentManager.h"
#include <iostream>
using namespace VE_XML;
//////////////////////////////
//Constructor               //
//////////////////////////////
XMLReader::XMLReader()
{
   _domDocumentManager = 0;
   _standAloneDDM = false;
}
///////////////////////
XMLReader::~XMLReader()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->UnLoadParser();
      if(_standAloneDDM)
      {
         delete _domDocumentManager;
         _domDocumentManager = 0;
      }
   }
   
}
/////////////////////////////////////////////////
void XMLReader::UseStandaloneDOMDocumentManager()
{
   if(!_domDocumentManager)
   {
      _domDocumentManager = new VE_Conductor::DOMDocumentManager();
      _standAloneDDM = true;
   }
}
//////////////////////////////
void XMLReader::WriteToFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLFileOn();
   }
}
//////////////////////////////
void XMLReader::WriteToString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLStringOn();
   }
}
//////////////////////////////
void XMLReader::ReadFromFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////
void XMLReader::ReadFromString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////////////////////////////////////////////////////////
void XMLReader::SetDOMDocumentManager(VE_Conductor::DOMDocumentManager* ddManager)
{
   _domDocumentManager = ddManager;
   _standAloneDDM = false;
}
////////////////////////////////////////////////////////////////////   
VE_Conductor::DOMDocumentManager* XMLReader::GetDOMDocumentManager()
{
   return _domDocumentManager;
}
////////////////////////////////////////////////
void XMLReader::ReadXMLData(std::string xmlData)
{
   _domDocumentManager->Load( xmlData );
   //_domDocumentManager->WriteAndReleaseCommandDocument();
   //override this in derived classes
   _populateStructureFromDocument(_domDocumentManager->GetCommandDocument());
}
