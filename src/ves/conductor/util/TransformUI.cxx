/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/util/TransformUI.h>
#include <ves/conductor/util/DataSetLoaderUI.h>

#include <ves/conductor/util/spinctld.h>

#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <wx/checkbox.h>

#include <vector>

using namespace ves::conductor::util;
using namespace ves::open::xml;

BEGIN_EVENT_TABLE( TransformUI, wxPanel )
    EVT_SPINCTRL( TRANSFORM_PANEL_ID, TransformUI::UpdateTransform )
    EVT_CHECKBOX( UNIFORM_SCALE,      TransformUI::UpdateUniformScale )
END_EVENT_TABLE()

TransformUI::TransformUI( wxWindow* parent, wxString dialogName, ves::open::xml::Transform* transform )
        : wxPanel( parent, TRANSFORM_PANEL_ID, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL )
{
    this->transform = transform;

    //_transformPanel = new wxPanel( parent,TRANSFORM_PANEL_ID );

    wxBoxSizer* transformPanelSizer = new wxBoxSizer( wxVERTICAL );
    wxStaticBox* transformProperties = new wxStaticBox( this, -1, dialogName );
    wxStaticBoxSizer* transformPropSizer = new wxStaticBoxSizer( transformProperties, wxVERTICAL );

    ///translation
    wxStaticBox* translation = new wxStaticBox( this, -1, wxT( "Translation (ft)" ) );
    wxStaticBoxSizer* transSizer = new wxStaticBoxSizer( translation, wxHORIZONTAL );
    _xTransformCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _xTransformCtrl->SetValue( 0 );
    _xTransformCtrl->SetRange( -1000.0, 1000.0 );
    _xTransformCtrl->SetIncrement( 1.0 );

    _yTransformCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _yTransformCtrl->SetValue( 0 );
    _yTransformCtrl->SetRange( -1000.0, 1000.0 );
    _yTransformCtrl->SetIncrement( 1.0 );

    _zTransformCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _zTransformCtrl->SetValue( 0 );
    _zTransformCtrl->SetRange( -1000.0, 1000.0 );
    _zTransformCtrl->SetIncrement( 1.0 );

    transSizer->Add( _xTransformCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    transSizer->Add( _yTransformCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    transSizer->Add( _zTransformCtrl, 1, wxALIGN_CENTER_HORIZONTAL );

    transformPropSizer->Add( transSizer, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );

    //rotation
    wxStaticBox* rotation = new wxStaticBox( this, -1, wxT( "Rotation (deg)" ) );
    wxStaticBoxSizer* rotationSizer = new wxStaticBoxSizer( rotation, wxHORIZONTAL );
    _xRotationCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _xRotationCtrl->SetValue( 0 );
    _xRotationCtrl->SetRange( -360.0, 360.0 );
    _xRotationCtrl->SetIncrement( 1.0 );

    _yRotationCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _yRotationCtrl->SetValue( 0 );
    _yRotationCtrl->SetRange( -360.0, 360.0 );
    _yRotationCtrl->SetIncrement( 1.0 );

    _zRotationCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _zRotationCtrl->SetValue( 0 );
    _zRotationCtrl->SetRange( -360.0, 360.0 );
    _zRotationCtrl->SetIncrement( 1.0 );

    rotationSizer->Add( _xRotationCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    rotationSizer->Add( _yRotationCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    rotationSizer->Add( _zRotationCtrl, 1, wxALIGN_CENTER_HORIZONTAL );

    transformPropSizer->Add( rotationSizer, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );

    //scale
    wxBoxSizer* scaleInfo = new wxBoxSizer( wxVERTICAL );
    wxStaticBox* scale = new wxStaticBox( this, -1, wxT( "Scale " ) );
    wxStaticBoxSizer* scaleSizer = new wxStaticBoxSizer( scale, wxHORIZONTAL );
    _xScaleCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _yScaleCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );
    _zScaleCtrl =  new wxSpinCtrlDbl( this, TRANSFORM_PANEL_ID );

    _xScaleCtrl->SetValue( 1.0 );
    _xScaleCtrl->SetRange( 0.0, 100.0 );
    _xScaleCtrl->SetIncrement( 1.0 );

    _yScaleCtrl->SetValue( 1.0 );
    _yScaleCtrl->SetRange( 0.0, 100.0 );
    _yScaleCtrl->SetIncrement( 1.0 );

    _zScaleCtrl->SetValue( 1.0 );
    _zScaleCtrl->SetRange( 0.0, 100.0 );
    _zScaleCtrl->SetIncrement( 1.0 );

    scaleSizer->Add( _xScaleCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    scaleSizer->Add( _yScaleCtrl, 1, wxALIGN_CENTER_HORIZONTAL );
    scaleSizer->Add( _zScaleCtrl, 1, wxALIGN_CENTER_HORIZONTAL );

    if( transform )
    {
        _xTransformCtrl->SetValue( transform->GetTranslationArray()->GetElement( 0 ) );
        _yTransformCtrl->SetValue( transform->GetTranslationArray()->GetElement( 1 ) );
        _zTransformCtrl->SetValue( transform->GetTranslationArray()->GetElement( 2 ) );

        _xScaleCtrl->SetValue( transform->GetScaleArray()->GetElement( 0 ) );
        _yScaleCtrl->SetValue( transform->GetScaleArray()->GetElement( 1 ) );
        _zScaleCtrl->SetValue( transform->GetScaleArray()->GetElement( 2 ) );

        _xRotationCtrl->SetValue( transform->GetRotationArray()->GetElement( 0 ) );
        _yRotationCtrl->SetValue( transform->GetRotationArray()->GetElement( 1 ) );
        _zRotationCtrl->SetValue( transform->GetRotationArray()->GetElement( 2 ) );
    }

    m_uniformScale = new wxCheckBox( this, UNIFORM_SCALE, wxT( "Uniform Scaling" ), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
    m_uniformScale->SetValue( true );

    scaleInfo->Add( scaleSizer, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    scaleInfo->Add( m_uniformScale, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );

    transformPropSizer->Add( scaleInfo, 2, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );

    transformPanelSizer->Add( transformPropSizer, 1, wxEXPAND | wxALIGN_CENTER );
    this->SetAutoLayout( true );
    this->SetSizer( transformPanelSizer );

    tempX = 1.0;
    tempY = 1.0;
    tempZ = 1.0;
//   paramBlock = 0;
}
///////////////////////////////////////////////////////////////////
TransformUI::~TransformUI( void )
{
    ;
}
///////////////////////////////////////////////////////////////////
void TransformUI::UpdateTransform( wxSpinEvent& WXUNUSED( event ) )
{
    if( _transform )
    {
        std::vector<double> temp;

        temp.push_back( _xTransformCtrl->GetValue() );
        temp.push_back( _yTransformCtrl->GetValue() );
        temp.push_back( _zTransformCtrl->GetValue() );
        _transform->GetTranslationArray()->SetArray( temp );
        std::cout << "YTRANSFORM :" << temp.at( 1 ) << std::endl;
        temp.clear();

        double xScale = _xScaleCtrl->GetValue();
        double yScale = _yScaleCtrl->GetValue();
        double zScale = _zScaleCtrl->GetValue();

        if( m_uniformScale->IsChecked() == true )
        {
            if( tempX != xScale )
            {
                double scaleBy = _xScaleCtrl->GetValue();
                _yScaleCtrl->SetValue( scaleBy );
                _zScaleCtrl->SetValue( scaleBy );
            }
            else if( tempY != yScale )
            {
                double scaleBy = _yScaleCtrl->GetValue();
                _xScaleCtrl->SetValue( scaleBy );
                _zScaleCtrl->SetValue( scaleBy );
            }
            else if( tempZ != zScale )
            {
                double scaleBy = _zScaleCtrl->GetValue();
                _xScaleCtrl->SetValue( scaleBy );
                _yScaleCtrl->SetValue( scaleBy );
            }
            temp.push_back( _xScaleCtrl->GetValue() );
            temp.push_back( _yScaleCtrl->GetValue() );
            temp.push_back( _zScaleCtrl->GetValue() );
        }
        else
        {
            temp.push_back( _xScaleCtrl->GetValue() );
            temp.push_back( _yScaleCtrl->GetValue() );
            temp.push_back( _zScaleCtrl->GetValue() );
        }
        _transform->GetScaleArray()->SetArray( temp );

        tempX = _xScaleCtrl->GetValue();
        tempY = _yScaleCtrl->GetValue();
        tempZ = _zScaleCtrl->GetValue();
        temp.clear();

        temp.push_back( _xRotationCtrl->GetValue() );
        temp.push_back( _yRotationCtrl->GetValue() );
        temp.push_back( _zRotationCtrl->GetValue() );
        _transform->GetRotationArray()->SetArray( temp );

        temp.clear();



        ves::open::xml::DataValuePair* paramBlockID = new ves::open::xml::DataValuePair();
        paramBlockID->SetData( std::string( "Parameter Block ID" ), _id );
        _instructions.push_back( paramBlockID );
//std::cout<<"NO"<<std::endl;}
        std::cout << "PARAMID :" << _id << std::endl;
        ves::open::xml::DataValuePair* updateTransform = new ves::open::xml::DataValuePair();
        updateTransform->SetData( "Transform", _transform );
        _instructions.push_back( updateTransform );
        std::cout << "TRANSFORM :" << std::endl;
//     paramBlock->GetTransform()
//      VE_XML::DataValuePair* updateTransform = new VE_XML::DataValuePair();
//      updateTransform->SetDataType("XMLOBJECT");
//      updateTransform->SetData("Transform",paramBlock->GetTransform());
//      _instructions.push_back(updateTransform);

//      _sendCommandsToXplorer();

        ///send command to xplorer
        ves::open::xml::Command* veCommand = new ves::open::xml::Command();
        veCommand->SetCommandName( "DATA_TRANSFORM_UPDATE" );
        veCommand->AddDataValuePair( paramBlockID );
        veCommand->AddDataValuePair( updateTransform );
        //   serviceList->SendCommandStringToXplorer( veCommand );

        ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );

        //clean up memory
        delete veCommand;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TransformUI::UpdateUniformScale( wxCommandEvent &event )
{}
////////////////////////////////////////////////////////////////////////////////
void TransformUI::SetParamBlockTransform( ves::open::xml::Transform* transform )
{
    _transform = transform;
}
////////////////////////////////////////////////////////////////////////////////
void TransformUI::_sendCommandsToXplorer()
{/*
                           VE_XML::Command* command=new VE_XML::Command();

                           for(size_t i=0;i<_instructions.size();i++)
                           {
                              command->AddDataValuePair(_instructions.at(i));
                           }
                           command->SetCommandName(_commandName);
                           serviceList->SendCommandStringToXplorer(command);
                           //Clean up memory
                           delete command;*/
}
////////////////////////////////////////////////////
void TransformUI::SetParamBlockID( std::string id )
{
    _id = id;
}

