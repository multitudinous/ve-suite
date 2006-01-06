#include "VE_Open/VE_XML/XMLReaderWriter.h"
#include "VE_Conductor/Framework/DOMDocumentManager.h"
#include <iostream>
using namespace VE_XML;
//////////////////////////////
//Constructor               //
//////////////////////////////
XMLReaderWriter::XMLReaderWriter()
{
   _domDocumentManager = 0;
   _standAloneDDM = false;
}
///////////////////////
XMLReaderWriter::~XMLReaderWriter()
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
void XMLReaderWriter::UseStandaloneDOMDocumentManager()
{
   if(!_domDocumentManager)
   {
      _domDocumentManager = new VE_Conductor::DOMDocumentManager();
      _standAloneDDM = true;
   }
}
//////////////////////////////
void XMLReaderWriter::WriteToFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLFileOn();
   }
}
//////////////////////////////
void XMLReaderWriter::WriteToString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLStringOn();
   }
}
//////////////////////////////
void XMLReaderWriter::ReadFromFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////
void XMLReaderWriter::ReadFromString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::SetDOMDocumentManager(VE_Conductor::DOMDocumentManager* ddManager)
{
   _domDocumentManager = ddManager;
   _standAloneDDM = false;
}
////////////////////////////////////////////////////////////////////   
VE_Conductor::DOMDocumentManager* XMLReaderWriter::GetDOMDocumentManager()
{
   return _domDocumentManager;
}
//////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData(std::string xmlData)
{
   _domDocumentManager->Load( xmlData );
   //override this in derived classes
   _populateStructureFromDocument(_domDocumentManager->GetCommandDocument());
}
////////////////////////////////////////
void XMLReaderWriter::WriteXMLDocument()
{
   if(_domDocumentManager){
      _domDocumentManager->WriteAndReleaseCommandDocument();
   }
}
