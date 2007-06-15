/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/XMLObject.h"

#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/XMLFormatter.hpp>

#include <iostream>
#include <fstream>

//using namespace VE_Conductor;
using namespace VE_XML;

//////////////////////////////////////////////////////
DOMDocumentManager::DOMDocumentManager( void )
{
   commandDocument = 0;
   parameterDocument = 0;
   modulesDocument = 0;
   parseXMLFile = false;
   writeXMLFile = false;
   parser = 0;
   errHandler = 0;
   outputXMLFile = std::string("./output.veg");
   documentType[ "Network" ] = std::pair< std::string, std::string >( "Network", "network" );
   documentType[ "Shader" ] = std::pair< std::string, std::string >( "Shader", "shader" );
   documentType[ "Command" ] = std::pair< std::string, std::string >( "Commands", "commands" );
}
//////////////////////////////////////////////////////
DOMDocumentManager::~DOMDocumentManager( void )
{
   outputXMLFile.clear();
}
//////////////////////////////////////////////////////
void DOMDocumentManager::InitializeXerces( void )
{
}
////////////////////////////////////////////
void DOMDocumentManager::SetWriteXMLFileOn()
{
   writeXMLFile = true;
}
//////////////////////////////////////////////
void DOMDocumentManager::SetWriteXMLStringOn()
{
   writeXMLFile = false;
}
////////////////////////////////////////////
void DOMDocumentManager::SetParseXMLFileOn()
{
   parseXMLFile = true;
}
//////////////////////////////////////////////
void DOMDocumentManager::SetParseXMLStringOn()
{
   parseXMLFile = false;
}
///////////////////////////////////////////////////////////////////
void DOMDocumentManager::SetOuputXMLFile(std::string xmlOutputFile)
{
   outputXMLFile = xmlOutputFile;
}
////////////////////////////////////////////////////////////////
void DOMDocumentManager::_readInputString(std::string xmlString)
{
   if ( xmlString.empty() )
   {
      return;
   }
   
   std::string system_id( "input.xml" );
   MemBufInputSource inputXML((const XMLByte*)xmlString.c_str(),
        static_cast< const unsigned int >( xmlString.size() ), system_id.c_str());
   parser->parse(inputXML);
}
////////////////////////////////////////////////////////////
void DOMDocumentManager::_readInputFile(std::string xmlFile)
{
   if ( !std::ifstream( xmlFile.c_str() ).good() )
   {
      std::cerr << "Could not open file : " << xmlFile.c_str() << std::endl;
      return;
   }
   
   //LocalFileInputSource inputXML(xercesString(xmlFile));
   //parser->parse(inputXML);
   parser->parse(xmlFile.c_str());
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

///////////////////////////////////////////////////////////////
void DOMDocumentManager::Load( const std::string inputCommand )
{
   //std::string system_id( "command.xml" );
   //MemBufInputSource inpsrc( (const XMLByte*)inputCommand.c_str(), inputCommand.size(), system_id.c_str());
  
   char* message;
   if(!parser)
   {
      parser = new XercesDOMParser();

      parser->setValidationScheme(XercesDOMParser::Val_Always);    // optional.
      parser->setDoNamespaces(true);    // optional
      errHandler = (ErrorHandler*) new HandlerBase();
      parser->setErrorHandler(errHandler);
   }

   try 
   {
      if(parseXMLFile)
      {
         _readInputFile(inputCommand);  
      }
      else
      {
         _readInputString(inputCommand);  
      }

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
      std::cout << "DOMDocumentManager::Load Unexpected Exception"  << std::endl;
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
void DOMDocumentManager::CreateCommandDocument( std::string type )
{
   DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation( xercesString( "LS" ) );

  char* message = 0;
   try
   {
	  std::map< std::string, std::pair< std::string, std::string > >::iterator iter = 
		   documentType.find( type );
	  std::string tempDoc = iter->second.second;
      commandDocument = impl->createDocument(0, xercesString( tempDoc ), 0);
   }
   catch(const XMLException &toCatch)
   {
      message = XMLString::transcode(toCatch.getMessage());
      std::cerr << "XMLException Exception message is: \n"
            << message << std::endl;
      XMLString::release( &message );
      return;
   }
    catch (const DOMException& toCatch) 
   {
      message = XMLString::transcode(toCatch.msg);
      std::cout << "DOMException Exception message is: \n"
		  << message << std::endl;
      XMLString::release(&message);
      return;
   }
  catch ( ... )
   {
      std::cerr << " ERROR : not a vaild document type : " << type << std::endl;
      return;
   }

   commandDocument->setVersion( xercesString( "1.0" ) );
   commandDocument->setEncoding( xercesString( "ISO-8859-1" ) );
   DOMElement* root_elem = commandDocument->getDocumentElement(); //This is the root element
   root_elem->setAttribute( xercesString( "name" ), xercesString( documentType[ type ].first ) );
   root_elem->setAttribute( xercesString( "xmlns:xsi" ), xercesString( "http://www.w3.org/2001/XMLSchema-instance" ) );
   root_elem->setAttribute( xercesString( "xsi:noNamespaceSchemaLocation" ), xercesString( "verg.xsd" ) );
   root_elem->setAttribute( xercesString( "xsi:noNamespaceSchemaLocation" ), xercesString( "verg_model.xsd" ) );
   root_elem->setAttribute( xercesString( "xsi:noNamespaceSchemaLocation" ), xercesString( "vecad.xsd" ) );
   root_elem->setAttribute( xercesString( "xsi:noNamespaceSchemaLocation" ), xercesString( "veshader.xsd" ) );
}
/////////////////////////////////////////////////////
std::string DOMDocumentManager::WriteAndReleaseCommandDocument( void )
{
   DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation( xercesString( "LS" ) );
   DOMWriter* theSerializer = dynamic_cast< DOMImplementationLS* >( impl )->createDOMWriter();
   theSerializer->setFeature( XMLUni::fgDOMWRTFormatPrettyPrint, true );
   char* message = 0;
   char* tempResultString = 0;
   std::string result;

   try 
   {
      if(writeXMLFile)
      {
         //take a string passed in as the filename to write out   
         LocalFileFormatTarget outputXML(outputXMLFile.c_str());
         theSerializer->writeNode(&outputXML,*commandDocument);
      }
      else
      {
         // do the serialization through DOMWriter::writeNode();
         XMLCh* xXml = theSerializer->writeToString( (*commandDocument) );
         tempResultString = XMLString::transcode( xXml );
         result = tempResultString;
         XMLString::release( &tempResultString );
         XMLString::release( &xXml );
      }
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
////////////////////////////////////////////////////////////////////////////////
/*void DOMDocumentManager::ProcessIncludes( void )
{
   //Process the include tags from documents
   //Does not matter whatorder includes are processed in
   // look at cppdom for example of how to process include
   // what have others done with xerces
   // the read and write of include tags will be handled by the 
   //individual xmlobjects like veNetwork, veModel
   // for a document this manager may handle this for external includes
   //The xml readerwirter would handle the write for the venetwork
   //to process models seperately
}*/
