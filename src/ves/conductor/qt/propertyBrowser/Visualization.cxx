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
#define QT_NO_KEYWORDS
// THIS HEADER MUST COME FIRST
// Header dynamically generated during build process by uic.
#include <ves/conductor/qt/propertyBrowser/ui_Visualization.h>

#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>

#include <ves/conductor/qt/VisFeatureManager.h>

#include <QtCore/qstring.h>

#include <iostream>

namespace ves
{
namespace conductor
{

Visualization::Visualization( QWidget* parent ) :
QDialog( parent ),
ui( new Ui::Visualization )
{
    ui->setupUi( this );

    mFeatureBrowser = new PropertyBrowser( this );
    mTempSet = 0;

    // Force FeatureIDSelector to update itself based on the database.
    UpdateFeatureIDSelectorChoices();
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
        mTempSet->WriteToDatabase( );
        std::string featureName = ui->FeaturesList->currentItem( )->text( ).toStdString( );
        VisFeatureManager::instance( )->UpdateFeature( featureName, mTempSet->GetRecordID( ) );
    }
}

void Visualization::on_RefreshPropertiesButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->LoadFromDatabase( );
        mFeatureBrowser->RefreshAll( );
    }
}

void Visualization::on_NewFeatureButton_clicked( )
{
    QString featureName = ui->FeaturesList->currentItem( )->text( );
    mTempSet = VisFeatureManager::instance( )->
            CreateNewFeature( featureName.toStdString( ) );

    if( mTempSet )
    {
        mTempSet->WriteToDatabase( );
    }

    mFeatureBrowser->ParsePropertySet( mTempSet );

    // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
    // to take the Qt-ized data from a PropertyBrowser such as
    // mFeatureBrowser and display it in the GUI.
    ui->vfpb->setPropertyBrowser( mFeatureBrowser );
    ui->vfpb->RefreshContents( );
    ui->vfpb->show( );

    // Re-read available feature IDs from database, and select the last one added,
    // which by definition corresponds to the "new" one
    UpdateFeatureIDSelectorChoices( );
    ui->FeatureIDSelector->setCurrentIndex( ui->FeatureIDSelector->count( ) - 1 );
}

void Visualization::on_DeleteFeatureButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->DeleteFromDatabase( );
        mFeatureBrowser->ParsePropertySet( 0 );
        delete mTempSet;
        mTempSet = 0;
        UpdateFeatureIDSelectorChoices( );
        ui->FeatureIDSelector->setCurrentIndex( ui->FeatureIDSelector->count( ) - 1 );
    }
}

void Visualization::on_FeaturesList_currentTextChanged( const QString& currentText )
{
    UpdateFeatureIDSelectorChoices( );
}

void Visualization::UpdateFeatureIDSelectorChoices( )
{
    // Remove all entries from feature id selection combobox
    ui->FeatureIDSelector->clear( );

    // Get all available IDs for current feature type
    QString featureName = ui->FeaturesList->currentItem( )->text( );
    std::vector<std::string> ids = VisFeatureManager::instance( )->GetIDsForFeature( featureName.toStdString( ) );

    // Convert IDs to format needed by QComboBox and add them as choices
    QStringList idList;
    QString qCurrentID;
    for( size_t index = 0; index < ids.size( ); index++ )
    {
        qCurrentID = qCurrentID.fromStdString( ids.at( index ) );
        idList.append( qCurrentID );
    }

    ui->FeatureIDSelector->addItems( idList );

    ui->FeatureIDSelector->setCurrentIndex( ui->FeatureIDSelector->count( ) - 1 );
}

void Visualization::on_FeatureIDSelector_currentIndexChanged( const QString & text )
{
    if( text.isEmpty( ) )
    {
        // If null selection was made, we want to remove any visible PropertySet
        mFeatureBrowser->ParsePropertySet( 0 );
        delete mTempSet;
        mTempSet = 0;
    }
    else
    {
        delete mTempSet;
        mTempSet = 0;
        QString featureName = ui->FeaturesList->currentItem( )->text( );
        mTempSet = VisFeatureManager::instance( )->
                CreateNewFeature( featureName.toStdString( ) );

        if( mTempSet )
        {
            mTempSet->SetRecordID( text.toInt( ) );
            mTempSet->LoadFromDatabase( );
            mFeatureBrowser->ParsePropertySet( mTempSet );

            // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
            // to take the Qt-ized data from a PropertyBrowser such as
            // mFeatureBrowser and display it in the GUI.
            ui->vfpb->setPropertyBrowser( mFeatureBrowser );
            ui->vfpb->RefreshContents( );
            ui->vfpb->show( );
            mTempSet->LoadFromDatabase( );
            mFeatureBrowser->RefreshAll( );
        }
    }

}

}// namespace conductor
}// namespace ves
