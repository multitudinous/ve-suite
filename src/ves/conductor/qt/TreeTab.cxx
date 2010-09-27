/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include <ves/conductor/qt/ui_TreeTab.h>

#include <ves/conductor/qt/TreeTab.h>
#include <osgQtTree/osgQtTree.h>
#include <osgQtTree/treemodel.h>

namespace ves
{
namespace conductor
{

TreeTab::TreeTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::TreeTab)
{
    ui->setupUi(this);

    ////////////// Making a Tree /////////////////////
    // Create a tree model and set the column headers on it
    mModel = new TreeModel;
    QList<QVariant> rootData;
    rootData << "Node Name";
    mModel->CreateRootItem( rootData );

    // Connect the tree model up to a tree view.
    ui->mTreeView->setModel( mModel );

}

TreeTab::~TreeTab()
{
    delete ui;
}

void TreeTab::changeEvent(QEvent *e)
{
    QWidget::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

void TreeTab::Clear()
{
    ui->mTreeView->setModel( 0 );
    delete mModel;
    mModel = new TreeModel;
    ui->mTreeView->setModel( mModel );
}

void TreeTab::PopulateWithRoot( osg::Node* root )
{
    mModel->BeginReset();
    
    mModel->Clear();
    QList<QVariant> rootData;
    rootData << "Node Name";
    mModel->CreateRootItem( rootData );
    
    // Create a node visitor that will add items into the tree model, and
    // turn it loose on the root node of the scenegraph
    osgQtTree::PopulateTreeControlWithNodeVisitor populator( mModel );
    root->accept( populator );

    mModel->EndReset();
}

QModelIndex TreeTab::OpenToAndSelect( osg::NodePath& nodepath )
{
    return osgQtTree::openToAndSelect( ui->mTreeView, mModel, nodepath );
}

} // namespace conductor
} // namespace ves
