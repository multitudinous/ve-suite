/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include <ves/xplorer/event/volume/TBUpdateScalarRangeEH.h>
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
TextureBasedUpdateScalarRangeEventHandler::TextureBasedUpdateScalarRangeEventHandler()
{}
///////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler
::TextureBasedUpdateScalarRangeEventHandler( const TextureBasedUpdateScalarRangeEventHandler& ceh )
{}
/////////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler::~TextureBasedUpdateScalarRangeEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedUpdateScalarRangeEventHandler&
TextureBasedUpdateScalarRangeEventHandler::operator=( const TextureBasedUpdateScalarRangeEventHandler& rhs )
{
    if( &rhs != this )
    {
        TextureBasedEventHandler::operator=( rhs );
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////
void TextureBasedUpdateScalarRangeEventHandler::_operateOnNode( XMLObjectPtr veXMLObject )
{
    try
    {
        CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateActiveTextureManager();

        double scalarRange[2] = {0.f, 100.f};

        DataValuePairPtr minScalarRange = command->GetDataValuePair( "Mininum Scalar Range" );
        minScalarRange->GetData( scalarRange[0] );

        DataValuePairPtr maxScalarRange = command->GetDataValuePair( "Maximum Scalar Range" );
        maxScalarRange->GetData( scalarRange[1] );

        //this is overkill
        float floatRange[2];
        floatRange[0] = scalarRange[0];
        floatRange[1] = scalarRange[1];
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateScalarRange( floatRange );

    }
    catch ( ... )
    {
        std::cout << "Invalid TextureDataSet!!" << std::endl;
        std::cout << "TextureBasedUpdateScalarRangeEventHandler::_operateOnNode()" << std::endl;
    }
}
