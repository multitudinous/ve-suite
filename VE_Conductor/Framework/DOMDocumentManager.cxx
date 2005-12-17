#include "VE_Conductor/Framework/DOMDocumentManager.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <iostream>

using namespace VE_Conductor;
using namespace VE_XML;

//////////////////////////////////////////////////////
DOMDocumentManager::DOMDocumentManager( void )
{
   commandDocument = 0;
   parameterDocument = 0;
   modulesDocument = 0;
}
//////////////////////////////////////////////////////
DOMDocumentManager::~DOMDocumentManager( void )
{
}
//////////////////////////////////////////////////////
void DOMDocumentManager::InitializeXerces( void )
{
}
//////////////////////////////////////////////////////
std::string DOMDocumentManager::WriteDocumentToString( DOMDocument* document )
{
   std::string outputData;
   // do all the xerces studd to make a DOMWriter
   return outputData;
}
//////////////////////////////////////////////////////
DOMDocument* DOMDocumentManager::GetCommandDocument( void )
{
   return commandDocument;
}
//////////////////////////////////////////////////////
DOMDocument* DOMDocumentManager::GetParameterDocument( void )
{
   return parameterDocument;
}
//////////////////////////////////////////////////////
DOMDocument* DOMDocumentManager::GetModulesDocument( void )
{
   return modulesDocument;
}
//////////////////////////////////////////////////////
void DOMDocumentManager::Load( const std::string inputCommand )
{
   std::string system_id( "command.xml" );
   MemBufInputSource inpsrc( (const XMLByte*)inputCommand.c_str(), inputCommand.size(), system_id.c_str());
  
   char* message;
   parser = new XercesDOMParser();

   parser->setValidationScheme(XercesDOMParser::Val_Always);    // optional.
   parser->setDoNamespaces(true);    // optional
   errHandler = (ErrorHandler*) new HandlerBase();
   parser->setErrorHandler(errHandler);

   try 
   {
      parser->parse(inpsrc);
   }
   catch (const XMLException& toCatch) 
   {
      message = XMLString::transcode(toCatch.getMessage());
      std::cout << "Exception message is: \n"
                  << message << "\n";
      XMLString::release( &message );
      delete parser;
      parser = 0;
      delete errHandler;
      errHandler = 0;

      return;
   }
   catch (const DOMException& toCatch) 
   {
      message = XMLString::transcode(toCatch.msg);
      std::cout << "Exception message is: \n"
                  << message << "\n";
      XMLString::release(&message);
      delete parser;
      parser = 0;
      delete errHandler;
      errHandler = 0;

      return;
   }
   catch (...) 
   {
      std::cout << "Unexpected Exception \n" ;
      delete parser;
      parser = 0;
      delete errHandler;
      errHandler = 0;
      return;
   }

  commandDocument = parser->getDocument(); //This is the rootNode;
}
//////////////////////////////////////////////////////
void DOMDocumentManager::UnLoadParser( void )
{
   if ( parser )
   {
      delete parser;
      parser = 0;
   }

   if ( errHandler )
   {
      delete errHandler;
      errHandler = 0;
   }
}
/////////////////////////////////////////////////////
void DOMDocumentManager::CreateCommandDocument( void )
{
  DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation( xercesString( "LS" ) );
 
  commandDocument = impl->createDocument(0, xercesString( "commands" ), 0);
  //DOMDocument* doc = impl->createDocument(0, xercesString( "commands" ), 0);
  commandDocument->setVersion( xercesString( "1.0" ) );
  commandDocument->setEncoding( xercesString( "ISO-8859-1" ) );
  DOMElement* root_elem = commandDocument->getDocumentElement(); //This is the root element
  root_elem->setAttribute( xercesString( "name" ), xercesString( "Commands" ) );
  root_elem->setAttribute( xercesString( "xmlns:xsi" ), xercesString( "http://www.w3.org/2001/XMLSchema-instance" ) );
  root_elem->setAttribute( xercesString( "xsi:noNamespaceSchemaLocation" ), xercesString( "verg.xsd" ) );
}
/////////////////////////////////////////////////////
std::string DOMDocumentManager::WriteAndReleaseCommandDocument( void )
{
   DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation( xercesString( "LS" ) );
   DOMWriter* theSerializer = dynamic_cast< DOMImplementationLS* >( impl )->createDOMWriter();
   theSerializer->setFeature( XMLUni::fgDOMWRTFormatPrettyPrint, true );
   char* message;
   std::string result;

   try 
   {
      // do the serialization through DOMWriter::writeNode();
      result = XMLString::transcode( theSerializer->writeToString( (*commandDocument) ) );
   }
   catch (const XMLException& toCatch) 
   {
      message = XMLString::transcode(toCatch.getMessage());
      std::cout << "Exception message is: \n"
                  << message << std::endl;
      XMLString::release( &message );
      //rv=false;
      return NULL;
   }
   catch (const DOMException& toCatch) 
   {
      message = XMLString::transcode(toCatch.msg);
      std::cout << "Exception message is: \n"
                  << message << std::endl;
      XMLString::release(&message);
      //rv=false;
      return NULL;
   }
   catch (...) 
   {
      std::cout << "Unexpected Exception " << std::endl;
      //rv=false;
      return NULL;
   }

   theSerializer->release();
   //rv=true;
   return result; 
}
