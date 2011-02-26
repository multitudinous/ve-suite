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
#define VES_DEBUG
#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

#include <ves/conductor/qt/propertyBrowser/ui_Visualization.h>

#include <ves/conductor/qt/VisFeatureManager.h>

#include <QtCore/qstring.h>

#include <iostream>

#include <boost/concept_check.hpp>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
Visualization::Visualization( QWidget* parent ) 
    :
    QDialog( parent ),
    m_ui( new Ui::Visualization ),
    mIgnoreIndexChange(false),
    m_logger( Poco::Logger::get("conductor.Visualization") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    m_ui->setupUi( this );

    mFeatureBrowser = new PropertyBrowser( this );
    LOG_TRACE( "ctor" );
}
////////////////////////////////////////////////////////////////////////////////
Visualization::~Visualization()
{
    LOG_TRACE( "dtor" );
    delete m_ui;
    delete mFeatureBrowser;
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::changeEvent( QEvent* e )
{
    QDialog::changeEvent( e );
    switch( e->type() )
    {
    case QEvent::LanguageChange:
        m_ui->retranslateUi( this );
        break;
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_WritePropertiesButton_clicked()
{
    if( mTempSet )
    {
        LOG_DEBUG( "on_WritePropertiesButton_clicked" );
        mTempSet->WriteToDatabase();
        //const std::string featureName = 
        //    m_ui->FeaturesList->currentItem()->text().toStdString();
        //VisFeatureManager::instance()->
        //    UpdateFeature( featureName, mTempSet->GetUUIDAsString() );

        mTempSet = ves::xplorer::data::PropertySetPtr();

        // Save off the list index so we can re-select this one after updating
        int lastKnownIndex = m_ui->FeatureIDSelector->currentIndex();
        // Update the choices in case user changed the Name Tag of a feature
        UpdateFeatureIDSelectorChoices();
        m_ui->FeatureIDSelector->setCurrentIndex( lastKnownIndex );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_RefreshPropertiesButton_clicked()
{
    if( mTempSet )
    {
        mTempSet->LoadFromDatabase();
        mFeatureBrowser->RefreshAll();
        LOG_DEBUG( "on_RefreshPropertiesButton_clicked" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_NewFeatureButton_clicked()
{
    // We create a stub property set of the correct type. The call to
    // UpdateFeatureIDSelectorChoices will cause this new set to be
    // discovered and loaded in the browser.
    LOG_DEBUG( "on_NewFeatureButton_clicked" );

    QString featureName = m_ui->FeaturesList->currentItem()->text();
    mTempSet = VisFeatureManager::instance()->
            CreateNewFeature( featureName.toStdString() );

    // Re-read available feature IDs from database, and select the last one added,
    // which corresponds to the "new" one
    UpdateFeatureIDSelectorChoices();
    m_ui->FeatureIDSelector->setCurrentIndex( m_ui->FeatureIDSelector->count() - 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_DeleteFeatureButton_clicked()
{
    if( mTempSet )
    {
        mTempSet->DeleteFromDatabase();
        ves::xplorer::data::PropertySetPtr nullPtr;
        mFeatureBrowser->ParsePropertySet( nullPtr );
        mTempSet = nullPtr;
        UpdateFeatureIDSelectorChoices();
        m_ui->FeatureIDSelector->setCurrentIndex( m_ui->FeatureIDSelector->count() - 1 );
        LOG_DEBUG( "on_DeleteFeatureButton_clicked" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_FeaturesList_currentTextChanged( const QString& currentText )
{
    LOG_DEBUG( "on_FeaturesList_currentTextChanged: " << currentText.toStdString() );
    ves::xplorer::data::PropertySetPtr nullPtr;
    mTempSet = nullPtr;
    UpdateFeatureIDSelectorChoices();
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::UpdateFeatureIDSelectorChoices()
{
    LOG_TRACE( "UpdateFeatureIDSelectorChoices" );
    // Remove all entries from feature id selection combobox. This will cause
    // a call to on_FeatureIDSelector_currentIndexChanged with index -1.
    m_ui->FeatureIDSelector->clear();

    // Drop all entries from uuid list
    m_ids.clear();

    // Get all available IDs for current feature type
    QString featureName = m_ui->FeaturesList->currentItem()->text();
    std::vector< std::pair< std::string, std::string > > nameIDPairs =
            VisFeatureManager::instance()->GetNameIDPairsForFeature(
                    featureName.toStdString() );

    // Get Name Tag for each feature and add these as choices
    QStringList nameList;
    QString qCurrentName;
    std::string tempName;
    std::string tempID;
    bool tempInList = false;
    for( size_t index = 0; index < nameIDPairs.size(); ++index )
    {
        tempName = nameIDPairs.at( index ).first;
        tempID = nameIDPairs.at( index ).second;

        // Check whether this entry refers to the set currently being edited
        if( mTempSet.get() )
        {
            if( mTempSet->GetUUIDAsString() == tempID  )
            {
                tempInList = true;
            }
        }

        qCurrentName = qCurrentName.fromStdString( tempName );
        m_ids.push_back( tempID );

        nameList.append( qCurrentName );
    }

    // If there an unsaved set being edited that isn't in the list, add it:
    if( mTempSet.get() && !tempInList )
    {
        LOG_TRACE( "UpdateFeatureIDSelectorChoices: adding temporary set to selection list" );
        qCurrentName = "Unsaved ";
        qCurrentName.append( m_ui->FeaturesList->currentItem()->text() );
        m_ids.push_back( mTempSet->GetUUIDAsString() );
        nameList.append( qCurrentName );
    }

    m_ui->FeatureIDSelector->addItems( nameList );
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_FeatureIDSelector_currentIndexChanged( int index )
{
    LOG_TRACE( "on_FeatureIDSelector_currentIndexChanged " << index );
    if(mIgnoreIndexChange)
    {
        mIgnoreIndexChange = false;
        return;
    }

    // Index of -1 is set by Qt when the list associated with this widget is
    // cleared. Index > size should only occur if code is faulty elsewhere
    // in this class. In either case we want to ensure nothing is visible in the
    // browser.
    if( (index == -1) || (index > int(m_ids.size()) ) )
    {
        LOG_TRACE( "on_FeatureIDSelector_currentIndexChanged: Removing any visible PropertySet" );
        mFeatureBrowser->ParsePropertySet( ves::xplorer::data::PropertySetPtr() );
        return;
    }

    LOG_TRACE( "on_FeatureIDSelector_currentIndexChanged: Loading PropertySet" );
    QString featureName = m_ui->FeaturesList->currentItem()->text();
    mTempSet = VisFeatureManager::instance()->
            CreateNewFeature( featureName.toStdString() );

    if( mTempSet )
    {
        mTempSet->SetUUID( m_ids.at( index ) );
        mTempSet->LoadFromDatabase();
        mFeatureBrowser->ParsePropertySet( mTempSet );

        // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
        // to take the Qt-ized data from a PropertyBrowser such as
        // mFeatureBrowser and display it in the GUI.
        m_ui->vfpb->setPropertyBrowser( mFeatureBrowser );
        m_ui->vfpb->RefreshContents();
        m_ui->vfpb->show();
    }

}
////////////////////////////////////////////////////////////////////////////////
}// namespace conductor
}// namespace ves
