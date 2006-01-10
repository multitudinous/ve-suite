/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: DOMDocumentManager.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef DOMDOCUMENT_MANAGER
#define DOMDOCUMENT_MANAGER
#include "VE_Installer/include/VEConfig.h"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <string>
XERCES_CPP_NAMESPACE_USE
namespace VE_XML
{
// This class should be made into a singleton
class VE_XML_EXPORTS DOMDocumentManager
{
public: 
   DOMDocumentManager( void );
   ~DOMDocumentManager( void );

   // Runs the intialization routines for xerces
   void InitializeXerces( void );

   // Writes the document that is passed in and returns a string
   std::string WriteDocumentToString( DOMDocument* document );

   // Get the respective documents for each portion of VE-Suite
   DOMDocument* GetCommandDocument( void );
   DOMDocument* GetParameterDocument( void );
   DOMDocument* GetModulesDocument( void );

   void Load( const std::string inputCommand );
   void UnLoadParser( void );

   ///Turn on the parsing of an XML file.
   void SetParseXMLFileOn();

   ///Turn on the parsing of an XML string.
   void SetParseXMLStringOn();

   ///Turn on the writing of an XML file.
   void SetWriteXMLFileOn();

   ///Turn on  the writing of an XML string
   void SetWriteXMLStringOn();

   // Functions used to create a document and then return it in a std::string
   std::string WriteAndReleaseCommandDocument( void );
   void CreateCommandDocument( void );

private:
    ///Read an input file.
   ///\param xmlFile The XML filename.
   void _readInputFile(std::string xmlFile);

   ///Read an input string.
   ///\param xmlString The XML filename.
   void _readInputString(std::string xmlString);

   bool parseXMLFile;///<Should we parse an XML file or string.
   bool writeXMLFile;///<Should we write an XML file or string.

   // This is the document used ot send commands to Xplorer
   DOMDocument* commandDocument;
   // This is the document used to send parameter file info
   // to Xplorer
   DOMDocument* parameterDocument;
   // This is the current nt file sent to the CE and is
   // constructed by Conductor
   DOMDocument* modulesDocument;

   XercesDOMParser* parser;
   ErrorHandler* errHandler;
};
}
#endif
