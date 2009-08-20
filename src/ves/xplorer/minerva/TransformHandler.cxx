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
 * Date modified: $Date: 2009-06-28 23:47:14 -0700 (Sun, 28 Jun 2009) $
 * Version:       $Rev: 12939 $
 * Author:        $Author: akubach $
 * Id:            $Id: AppFrame.cxx 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for set geographic properties command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/TransformHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/TransformPtr.h>

#include <ves/util/commands/Minerva.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

TransformHandler::TransformHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

TransformHandler::~TransformHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  
//
///////////////////////////////////////////////////////////////////////////////

void TransformHandler::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr nodeIDData ( command->GetDataValuePair( "Node ID" ) );
  ves::open::xml::DataValuePairPtr nodeType ( command->GetDataValuePair( "Node Type" ) );
  ves::open::xml::DataValuePairPtr transformData ( command->GetDataValuePair( "Transform" ) );

  /// Assembly not handled yet.
  if ( "Assembly" == nodeType->GetDataString() )
  {
    return;
  }

  std::string nodeId;
  nodeIDData->GetData ( nodeId );

  ves::open::xml::TransformPtr transform ( boost::dynamic_pointer_cast<ves::open::xml::Transform> ( transformData->GetDataXMLObject() ) );

  ModelWrapper::RefPtr modelWrapper ( this->GetOrCreateModel ( nodeId, manager ) );

  ves::open::xml::FloatArrayPtr scaleArray ( transform->GetScaleArray() );
  ves::open::xml::FloatArrayPtr rotationArray ( transform->GetRotationArray() );

  modelWrapper->scale ( osg::Vec3d ( scaleArray->GetElement ( 0 ), scaleArray->GetElement ( 1 ), scaleArray->GetElement ( 2 ) ) );
  modelWrapper->orientation ( rotationArray->GetElement ( 0 ), rotationArray->GetElement ( 1 ), rotationArray->GetElement ( 2 ) );

  // TODO: Handle translation from cad transform.

  manager.UpdateModel ( modelWrapper.get() );
}
