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
 * File:          $RCSfile: VEModel.h,v $
 * Date modified: $Date: 2006-01-14 18:41:24 -0600 (Sat, 14 Jan 2006) $
 * Version:       $Rev: 3503 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_MODEL_H_
#define _VE_MODEL_H_
/*!\file VEModel.h
  *Model API
  */

/*!\class VE_XML::VEModel
 *Class that manages the model info for all of VE-Suite.
 */
#include <string>
#include <vector>
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
   class VEPort;
   class VEDataValuePair;
   class VEParameterBlock;
   class VEPoint;
}

namespace VE_CAD
{
   class CADNode;
}

namespace VE_XML
{
class VE_XML_EXPORTS VEModel : public VEXMLObject
{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument
   VEModel( DOMDocument* rootDoc );
   ///Destructor
   virtual ~VEModel();
   ///Copy Constructor
   VEModel( const VEModel& );
   ///equal operator
   VEModel& operator= ( const VEModel& );
   
   ///Set the model name
   ///\param name name of the model
   void SetModelName( std::string name );
   ///Set the unique model id
   ///\param id id of the model
   void SetModelID( unsigned int id );
   ///Set the icon filename 
   ///\param filename filename for the icon to be loaded
   void SetIconFilename( std::string filename );
   
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Get point for the icon location
   VEPoint* GetIconLocation( void );
   ///Get results data
   VEDataValuePair* GetResult( unsigned int i );
   ///Get results data
   unsigned int GetNumberOfResults( void );
   ///Get input data
   VEDataValuePair* GetInput( unsigned int i );
   ///Get input data
   unsigned int GetNumberOfInputs( void );
   ///Get the i'th port for the model.
   ///\param i The i'th port you are after.
   VEPort* GetPort( unsigned int i );
   ///Get port data
   unsigned int GetNumberOfPorts( void );
   ///Get the i'th information packet for a model.
   ///\param i The i'th packet you are after.
   VEParameterBlock* GetInformationPacket( unsigned int i );
   ///Get info packets data
   unsigned int GetNumberOfInformationPackets( void );
   ///Get the geometry for the model.
   VE_CAD::CADNode* GetGeometry( void );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

private:
   ///raw datatypes of VEModel that are specified in the verg_model.xsd file
   std::string modelName;///<The name of the model.
   unsigned int uniqueModelID;///<The unique model id. 
   std::string iconFileName;///<The filename for the icon if any at all. Can possibly use an which would be compiled at runtime.
   std::vector< VEPort* > ports;///<The vector port data if any for a model.
   VEPoint* iconLocation;///<The icon location point container.
   ///The data value pair will contain all the results for a paticular model
   std::vector< VEDataValuePair* > results;///<The classes hold the results for the model.
   ///The data value pair will contain the model inputs for the model
   std::vector< VEDataValuePair* > inputs;///<The classes hold the inputs for the model.
   ///The parameter block holds all the data the was formerly stored in the param file
   std::vector< VEParameterBlock* > informationPackets;///<The classes hold relevant data to represent the model.
   ///The CADNode contains the tree structure for the geometry
   VE_CAD::CADNode* geometry;///<The classes hold the geometry for the model.
};
}
#endif// _VE_MODEL_H_
