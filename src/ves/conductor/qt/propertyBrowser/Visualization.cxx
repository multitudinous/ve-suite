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
#define QT_NO_KEYWORDS
// THIS HEADER MUST COME FIRST
// Header dynamically generated during build process by uic. DO NOT INSERT PATH
// INFORMATION!
#include <ves/conductor/qt/propertyBrowser/ui_Visualization.h>

#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>

#include <iostream>

using namespace ves::conductor;

Visualization::Visualization( QWidget* parent ) :
QDialog( parent ),
ui( new Ui::Visualization )
{
    ui->setupUi( this );

    mFeatureBrowser = new PropertyBrowser( this );
    mTempSet = NULL;

    // !!! This is just for testing purposes!!!
    mDbName = "/home/penn/vestTest.db";
}

Visualization::~Visualization( )
{
    delete ui;
    delete mFeatureBrowser;
    delete mTempSet;
}

void Visualization::changeEvent( QEvent* e )
{
    QDialog::changeEvent( e );
    switch( e->type( ) )
    {
    case QEvent::LanguageChange:
        ui->retranslateUi( this );
        break;
    default:
        break;
    }
}

void Visualization::on_WritePropertiesButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->WriteToDatabase( mDbName );
    }

    // featureType will be the list position of the currently-selected feature
    //VisFeatureManager::instance()->update( featureType, id );

    mContourFeatureMaker.update( mDbName, mTempSet->GetRecordID() );
}

void Visualization::on_RefreshPropertiesButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->LoadFromDatabase( mDbName );
        mFeatureBrowser->RefreshAll( );
    }
}

void Visualization::on_NewFeatureButton_clicked( )
{
    mTempSet = new xplorer::data::ContourPlanePropertySet( );

    if( mTempSet )
    {
        mTempSet->WriteToDatabase( mDbName );
        mFeatureBrowser->ParsePropertySet( mTempSet );

        // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
        // to take the Qt-ized data from a PropertyBrowser such as
        // mFeatureBrowser and display it in the GUI.
        ui->vfpb->setPropertyBrowser( mFeatureBrowser );
        ui->vfpb->RefreshContents( );
        ui->vfpb->show( );
    }
}

void Visualization::on_DeleteFeatureButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->DeleteFromDatabase( mDbName );
        mFeatureBrowser->ParsePropertySet( NULL );
        delete mTempSet;
        mTempSet = NULL;
    }
}
