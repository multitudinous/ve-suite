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
 * Id:            $Id: LayersTree.cxx 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifdef MINERVA_GIS_SUPPORT
#include <ves/conductor/qt/minerva/LayersTree.h>

#include <Minerva/Qt/Widgets/LayersTree.h>

#include <QtGui/QVBoxLayout>

using namespace ves::conductor::qt::minerva;

LayersTree::LayersTree ( QWidget *parent ) : BaseClass ( parent ),
  mLayersTree ( 0x0 )
{
  mLayersTree = new Minerva::QtWidgets::LayersTree;

  QVBoxLayout *layout ( new QVBoxLayout );
  layout->addWidget ( mLayersTree );

  this->setLayout ( layout );
}

LayersTree::~LayersTree()
{
  delete mLayersTree;
  mLayersTree = 0x0;
}

void LayersTree::buildTree ( Minerva::Core::Data::Feature * feature )
{
  mLayersTree->buildTree ( feature );
}
#endif