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

#include <ves/xplorer/event/volume/TBActivateEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/Debug.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/DataValuePair.h>
using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler::TextureBasedActivateEventHandler()
{}
///////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler
::TextureBasedActivateEventHandler( const TextureBasedActivateEventHandler& ceh )
{}
/////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler::~TextureBasedActivateEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
TextureBasedActivateEventHandler&
TextureBasedActivateEventHandler::operator=( const TextureBasedActivateEventHandler& rhs )
{
    if( &rhs != this )
    {
        TextureBasedEventHandler::operator=( rhs );
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////
void TextureBasedActivateEventHandler::_operateOnNode( XMLObject* veXMLObject )
{
    try
    {
        if( _activeModel )
        {
            Command* command = dynamic_cast< Command* >( veXMLObject );
            DataValuePairWeakPtr datasetName = command->GetDataValuePair( "Active Dataset Name" );

            std::string activeDatasetName;
            datasetName->GetData( activeDatasetName );

            DataSet* dataSet = _activeModel->GetCfdDataSet(
                      _activeModel->GetIndexOfDataSet( activeDatasetName ) );

            _activeModel->SetActiveDataSet( dataSet );

            //make the CAD transparent
            _activeModel->GetModelCADHandler()->MakeCADRootTransparent();
            if( !_activeModel->GetDCS()->SearchChild( _activeModel->GetActiveDataSet()->GetDCS() ) )
            {
                vprDEBUG( vesDBG, 2 ) << "|\t\tadding active switch node to worldDCS"
                << std::endl << vprDEBUG_FLUSH;
                _activeModel->GetDCS()->AddChild( _activeModel->GetActiveDataSet()->GetDCS() );
            }
            ves::xplorer::scenegraph::Switch* temp = _activeModel->GetActiveDataSet()->GetSwitchNode();
            if( !_activeModel->GetActiveDataSet()->GetDCS()->SearchChild( temp ) )
            {
                vprDEBUG( vesDBG, 2 ) << "|\t\tadding active dcs node to worldDCS for classic ss "
                << std::endl << vprDEBUG_FLUSH;
                _activeModel->GetActiveDataSet()->GetDCS()->AddChild( temp );
            }
            ///what happens if texture is somehow added first? Is that possible?
            _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal( 1 );
            _activeTDSet = _activeModel->GetTextureDataSet( 0 );
            ves::xplorer::TextureBasedVizHandler::instance()->SetActiveTextureDataSet( _activeTDSet );

        }
    }
    catch ( ... )
    {
        std::cout << "Invalid Model!!" << std::endl;
        std::cout << "TextureBasedActivateEventHandler::_operateOnNode()" << std::endl;
    }

}
