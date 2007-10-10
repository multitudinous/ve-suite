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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef DOMDOCUMENT_MANAGER
#define DOMDOCUMENT_MANAGER
/*!\file DOMDocumentManager.h
  Handler for DOMDocuments
  */
/*!\class VE_XML::DOMDocumentManager
 *This class manages the main documents for VE-Open.
 */
#include "VE_Installer/include/VEConfig.h"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <string>
#include <map>
#include <utility>

XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
// This class should be made into a singleton
class VE_XML_EXPORTS DOMDocumentManager
{
public: 
   ///Constructor
   DOMDocumentManager( void );
   ///Destructor
   ~DOMDocumentManager( void );

   /// Runs the intialization routines for xerces
   void InitializeXerces( void );

   /// Writes the document that is passed in and returns a string
   std::string WriteDocumentToString( DOMDocument* document );

   /// Get the command document
   DOMDocument* GetCommandDocument( void );

   /// Get the parameter document
   ///\todo This is not implemented yet
   DOMDocument* GetParameterDocument( void );

   /// Get the modules document
   ///\todo This is not implemented yet
   DOMDocument* GetModulesDocument( void );

   ///Load an XML tree from a string or a file. 
   ///\param inputCommand Can be a string in memory of the tree or point to a file on disk.
   void Load( const std::string inputCommand );
   ///Unload memory created by the parser.
   void UnLoadParser( void );

   ///Turn on the parsing of an XML file.
   void SetParseXMLFileOn();

   ///Turn on the parsing of an XML string.
   void SetParseXMLStringOn();

   ///Turn on the writing of an XML file.
   void SetWriteXMLFileOn();

   ///Turn on  the writing of an XML string
   void SetWriteXMLStringOn();
   
   ///Set the output file to write the XML document to.
   ///\param xmlOutputFile The full path and name to write the XML document to.
   void SetOuputXMLFile(std::string xmlOutputFile);

   /// Functions used to create a document and then return it in a std::string
   std::string WriteAndReleaseCommandDocument( void );
   ///Create the command document.
   ///\param type The type of document to be encoded in the header of the xml document either: Network, Shader, Command
   void CreateCommandDocument( std::string type );

private:
    std::string outputXMLFile;///<The output XML file to write.

   ///Read an input file.
   ///\param xmlFile The XML filename.
   void _readInputFile(std::string xmlFile);

   ///Read an input string.
   ///\param xmlString The XML filename.
   void _readInputString(std::string xmlString);

   bool parseXMLFile;///<Should we parse an XML file or string.
   bool writeXMLFile;///<Should we write an XML file or string.

   
   DOMDocument* commandDocument;///< This is the document used ot send commands to Xplorer
   
   DOMDocument* parameterDocument;///< This is the document used to send parameter file info to Xplorer

   DOMDocument* modulesDocument;///< This is the current Network file sent to the CE and is constructed by Conductor

   XercesDOMParser* parser;///<The DOMParser
   ErrorHandler* errHandler;///<The error handler for the parser.

   std::map< std::string, std::pair< std::string, std::string > > documentType;
};
}
#endif
