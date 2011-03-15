/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifdef MINERVA_GIS_SUPPORT
#include <ves/conductor/qt/minerva/LayersTree.h>

#include <Minerva/Qt/Widgets/TreeControl.h>
#include <Minerva/Qt/Widgets/TreeNode.h>

#include <QtGui/QFileDialog>
#include <QtGui/QVBoxLayout>
#include <QtGui/QMenu>
#include <QtGui/QMouseEvent>

#include <boost/shared_ptr.hpp>

#include <iostream>

using namespace ves::conductor::qt::minerva;

LayersTree::LayersTree ( QWidget *parent ) : BaseClass ( parent ),
  mTreeControl ( 0x0 )
{
    mTreeControl = new Minerva::QtWidgets::TreeControl ( this );

    QVBoxLayout *layout ( new QVBoxLayout );
    layout->addWidget ( mTreeControl );

    this->setContextMenuPolicy ( Qt::CustomContextMenu );
    QObject::connect ( this, SIGNAL ( customContextMenuRequested ( const QPoint& ) ), this,  SLOT   ( _onContextMenuShow ( const QPoint& ) ) );

    this->setLayout ( layout );
}

LayersTree::~LayersTree()
{
    delete mTreeControl;
    mTreeControl = 0x0;
}

void LayersTree::buildTree ( Minerva::Core::Data::Feature * feature )
{
    mTreeControl->buildTree ( feature );
}

void LayersTree::_onContextMenuShow ( const QPoint& pos )
{
    if ( 0x0 == mTreeControl )
        return;
  
    Minerva::QtWidgets::TreeNode *currentItem ( mTreeControl->currentNode() );
  
    if ( 0x0 == currentItem )
        return;
  
    Minerva::QtWidgets::TreeNode *parentItem ( currentItem->parent() );
  
    Minerva::Core::Data::Feature::RefPtr unknown ( currentItem->node().get() );
    Minerva::Core::Data::Feature::RefPtr parent ( 0x0 != parentItem ? parentItem->node().get() : 0x0 );
    bool hasParent ( parent && parent->asContainer() );

    QMenu* menu ( new QMenu ( this ) );
  
    QAction* addLayerAction ( new QAction ( QString ( "Add..." ), 0x0 ) );
    QObject::connect ( addLayerAction, SIGNAL ( triggered (bool) ), this, SLOT ( _addLayer() ) );
    menu->addAction ( addLayerAction );
  
    menu->popup ( mTreeControl->mapToGlobal ( pos ) );
}

void LayersTree::_addLayer()
{
    emit addLayerRequested();
}

#endif
