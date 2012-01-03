/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <ves/xplorer/event/volume/TBBBoxEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
TextureBasedBoundingBoxEventHandler::TextureBasedBoundingBoxEventHandler()
{}
///////////////////////////////////////////////////////////////////
TextureBasedBoundingBoxEventHandler
::TextureBasedBoundingBoxEventHandler( const TextureBasedBoundingBoxEventHandler& ceh )
    :
    TextureBasedEventHandler( ceh )
{}
/////////////////////////////////////////////////////////////////////
TextureBasedBoundingBoxEventHandler::~TextureBasedBoundingBoxEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedBoundingBoxEventHandler&
TextureBasedBoundingBoxEventHandler::operator=( const TextureBasedBoundingBoxEventHandler& rhs )
{
    if( &rhs != this )
    {
        TextureBasedEventHandler::operator=( rhs );
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////
void TextureBasedBoundingBoxEventHandler::_operateOnNode( XMLObjectPtr veXMLObject )
{
    try
    {
        CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
        DataValuePairPtr bboxFlag = command->GetDataValuePair( "BBox Flag" );
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateBoundingBox(( bboxFlag->GetUIntData() == 1 ) ? true : false );

    }
    catch ( ... )
    {
        std::cout << "Invalid TextureDataSet!!" << std::endl;
        std::cout << "TextureBasedBoundingBoxEventHandler::_operateOnNode()" << std::endl;
    }
}
