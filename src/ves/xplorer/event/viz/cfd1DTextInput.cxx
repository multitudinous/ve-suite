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

#include <ves/xplorer/event/viz/cfd1DTextInput.h>

#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkVectorText.h>
#include <vtkProperty.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfd1DTextInput::cfd1DTextInput( void ): DCS()
{
    this->geode = new ves::xplorer::scenegraph::Geode();
    (( ves::xplorer::scenegraph::DCS* )this )->AddChild( this->geode.get() );

    // Do we need to initialize this->text here?
    // If this->text is required, it should probably be a constructor argument
}

cfd1DTextInput::~cfd1DTextInput( void )
{
    //vprDEBUG(vesDBG,2) << "cfd1DTextInput Destructor: doing nothing"
    //                       << std::endl << vprDEBUG_FLUSH;
}

ves::xplorer::scenegraph::DCS* cfd1DTextInput::getpfDCS( void )
{
    return ( ves::xplorer::scenegraph::DCS* )this;
}

void cfd1DTextInput::SetFilename( std::string text )
{
    this->text = text;
    vprDEBUG( vesDBG, 1 ) << "\tcfd1DTextInput : " << this->text
    << std::endl << vprDEBUG_FLUSH;
}

void cfd1DTextInput::SetTransforms( double scale[ 3 ],
                                    double trans[ 3 ],
                                    double rot[ 3 ] )
{
    this->SetScaleArray( scale );
    this->SetTranslationArray( trans );
    this->SetRotationArray( rot );
}

void cfd1DTextInput::UpdateTextColor( double redColor, double greenColor, double blueColor )
{
    vtkVectorText* labelScalar       = vtkVectorText::New();
    vtkPolyDataMapper *labelMapper   = vtkPolyDataMapper::New();
    actor                            = vtkActor::New();

    labelScalar->SetText( text.c_str() );
    labelMapper->SetInput( labelScalar->GetOutput() );
    actor->SetMapper( labelMapper );
    this->actor->GetProperty()->SetSpecularPower( 20.0f );
    double color[ 3 ];
    color[ 0 ] = redColor;
    color[ 1 ] = greenColor;
    color[ 2 ] = blueColor;
    this->actor->GetProperty()->SetColor( color );

    this->geode->TranslateToGeode( actor );

    actor->Delete();
    labelMapper->Delete();
    labelScalar->Delete();
}

void cfd1DTextInput::Update( void )
{
    vtkVectorText* labelScalar = vtkVectorText::New();
    labelScalar->SetText( this->text.c_str() );

    vtkPolyDataMapper *labelMapper = vtkPolyDataMapper::New();
    labelMapper->SetInput( labelScalar->GetOutput() );

    vtkActor * actor = vtkActor::New();
    actor->SetMapper( labelMapper );
    actor->GetProperty()->SetSpecularPower( 20.0f );

    // set text color to green
    double color[ 3 ];
    color[ 0 ] = color[ 2 ] = 0;
    color[ 1 ] = 1;
    actor->GetProperty()->SetColor( color );

    this->geode->TranslateToGeode( actor );

    actor->Delete();
    labelMapper->Delete();
    labelScalar->Delete();
}

