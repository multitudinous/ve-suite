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
#include <ves/xplorer/event/volume/TextureBasedEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer::volume;
using namespace ves::open::xml;

//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
TextureBasedEventHandler::TextureBasedEventHandler()
        : ves::xplorer::event::EventHandler()
{
    _activeModel = 0;
    _activeTDSet = 0;
}
////////////////////////////////////////////////////////////
TextureBasedEventHandler::TextureBasedEventHandler( const TextureBasedEventHandler& rhs )
        : ves::xplorer::event::EventHandler()
{
    _activeModel = rhs._activeModel;
    _activeTDSet = rhs._activeTDSet;
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
TextureBasedEventHandler::~TextureBasedEventHandler()
{}
///////////////////////////////////////////////////////////////////////////
void TextureBasedEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    try
    {
        if( model )
        {
            _activeModel = dynamic_cast<ves::xplorer::Model*>( model );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch ( ... )
    {
        _activeModel = 0;
        _activeTDSet = 0;
        std::cout << "Invalid object passed to TextureBasedEventHandler!" << std::endl;
    }
}
///////////////////////////////////////////////////////
///Exectute the event                                //
//////////////////////////////////////////////////////////////////////
void TextureBasedEventHandler::Execute( XMLObject* veXMLObject )
{
    //this is ridiculously simple now...Just testing to see how things will work.
    //Command* command = dynamic_cast< Command* >( xmlObject );
    if( _activeModel )
    {
        try
        {
            _setActiveTextureDataset( /*name of the active texture dataset*/ );
        }
        catch ( ... )
        {
            std::cout << "Invalid texture dataset!!" << std::endl;
            std::cout << "TextureBasedEventHandler::Execute()" << std::endl;
        }
        /*try
        {
           _setActiveSolutionName(veXMLObject);
        }
        catch(...)
        {
            std::cout<<"Invalid texture dataset!!"<<std::endl;      
            std::cout<<"TextureBasedEventHandler::Execute()"<<std::endl;      
        }
        try
        {
           DataValuePair* bboxState = command->GetDataValuePair( "Bounding Box" );
         if(bboxState)
           {
              bool showBBox = false;
              unsigned int state = 0;
              bboxState->GetData( state );
              _ensureBBoxState(((state==0)?false:true));
         }
        }
        catch(...)
        {
            std::cout<<"Invalid texture dataset!!"<<std::endl;      
            std::cout<<"TextureBasedEventHandler::Execute()"<<std::endl;      
        }*/

        //this is overridden in derived classes
        _operateOnNode( veXMLObject );
    }
}
///////////////////////////////////////////////////////////////////////
TextureBasedEventHandler& TextureBasedEventHandler::operator=( const TextureBasedEventHandler& rhs )
{
    if( this != &rhs )
    {
        _activeModel = rhs._activeModel;
    }
    return *this;
}
//////////////////////////////////////////////////////////////
/*void TextureBasedEventHandler::_ensureBBoxState(bool showBBox)
{
}*/
/////////////////////////////////////////////////////////
void TextureBasedEventHandler::_setActiveTextureDataset()
{
    if( _activeModel )
    {
        //This assumes there is only one texture dataset per model which isn't quite right---biv
        _activeTDSet = _activeModel->GetTextureDataSet( 0 );
        _activeModel->SetActiveTextureDataSet( _activeTDSet );
        ves::xplorer::TextureBasedVizHandler::instance()->SetActiveTextureDataSet( _activeTDSet );

    }
}
