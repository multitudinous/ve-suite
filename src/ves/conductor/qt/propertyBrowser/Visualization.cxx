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
#define QT_NO_KEYWORDS
// THIS HEADER MUST COME FIRST
// Header dynamically generated during build process by uic.
#include <ves/conductor/qt/propertyBrowser/ui_Visualization.h>

#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

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

    // Force FeatureIDSelector to update itself based on the database.
    //UpdateFeatureIDSelectorChoices();
}
////////////////////////////////////////////////////////////////////////////////
Visualization::~Visualization()
{
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
        mTempSet->WriteToDatabase();
        const std::string featureName = 
            m_ui->FeaturesList->currentItem()->text().toStdString();
        VisFeatureManager::instance()->
            UpdateFeature( featureName, mTempSet->GetUUIDAsString() );
        // Update the choices in case user changed the Name Tag of a feature
        UpdateFeatureIDSelectorChoices();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_RefreshPropertiesButton_clicked()
{
    if( mTempSet )
    {
        mTempSet->LoadFromDatabase();
        mFeatureBrowser->RefreshAll();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_NewFeatureButton_clicked()
{
    // We create a stub property set of the correct type and write it out to
    // the database to ensure it is given a unique record id. The call to
    // UpdateFeatureIDSelectorChoices will cause this new record to be
    // discovered and loaded as the active property set.

    QString featureName = m_ui->FeaturesList->currentItem()->text();
    mTempSet = VisFeatureManager::instance()->
            CreateNewFeature( featureName.toStdString() );
    if( mTempSet )
    {
        mTempSet->WriteToDatabase();
    }

    // Re-read available feature IDs from database, and select the last one added,
    // which by definition corresponds to the "new" one
    UpdateFeatureIDSelectorChoices();
    m_ui->FeatureIDSelector->setCurrentIndex( m_ui->FeatureIDSelector->count() - 1 );
    LOG_DEBUG( "on_NewFeatureButton_clicked" );
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
    boost::ignore_unused_variable_warning( currentText );
    UpdateFeatureIDSelectorChoices();
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::UpdateFeatureIDSelectorChoices()
{
    // The problem with this block is that it assumes the featureName has not
    // changed -- and it may actually have changed. If the featureName has changed,
    // we don't want to do any fancy ignoring of stuff.
//    // If there are already ids loaded, we set a temporary flag so that after
//    // the call to FeatureIDSelector->clear() we can tell
//    // on_FeatureIDSelector_currentIndexChanged to ignore the index change
//    // that is triggered by the call to addItems. We only want it to load a
//    // property set after we explicitly call setCurrentIndex.
//    bool tIgnore = false;
//    if( m_ui->FeatureIDSelector->count() > 1 )
//    {
//        tIgnore = true;
//    }

    // Remove all entries from feature id selection combobox
    m_ui->FeatureIDSelector->clear();

    // Drop all entries from uuid list
    m_ids.clear();

//    mIgnoreIndexChange = tIgnore;

    // Get all available IDs for current feature type
    QString featureName = m_ui->FeaturesList->currentItem()->text();
    std::vector< std::pair< std::string, std::string > > nameIDPairs =
            VisFeatureManager::instance()->GetNameIDPairsForFeature(
                    featureName.toStdString() );

    // Get Name Tag for each feature and add these as choices
    QStringList nameList;
    //QString qCurrentID;
    QString qCurrentName;
    std::string tempName;
    std::string tempID;
    for( size_t index = 0; index < nameIDPairs.size(); ++index )
    {
        tempName = nameIDPairs.at( index ).first;
        tempID = nameIDPairs.at( index ).second;

        qCurrentName = qCurrentName.fromStdString( tempName );
        m_ids.push_back( tempID );

        nameList.append( qCurrentName );
    }

    m_ui->FeatureIDSelector->addItems( nameList );

    //m_ui->FeatureIDSelector->setCurrentIndex( m_ui->FeatureIDSelector->count() - 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_FeatureIDSelector_currentIndexChanged( int index )
{
    if(mIgnoreIndexChange)
    {
        mIgnoreIndexChange = false;
        return;
    }
    ves::xplorer::data::PropertySetPtr nullPtr;

    if( (index == -1) || (index > int(m_ids.size()) ) )
    {
        // If null selection was made, we want to remove any visible PropertySet
        mFeatureBrowser->ParsePropertySet( nullPtr );
        mTempSet = nullPtr;
    }
    else
    {
        mTempSet = nullPtr;
        QString featureName = m_ui->FeaturesList->currentItem()->text();
        mTempSet = VisFeatureManager::instance()->
                CreateNewFeature( featureName.toStdString() );

        if( mTempSet )
        {
            mFeatureBrowser->ParsePropertySet( mTempSet );

            // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
            // to take the Qt-ized data from a PropertyBrowser such as
            // mFeatureBrowser and display it in the GUI.
            m_ui->vfpb->setPropertyBrowser( mFeatureBrowser );
            m_ui->vfpb->RefreshContents();
            m_ui->vfpb->show();
            // No need to load before parsing, since values in browser are not
            // set during parsing. They're only set by signals from the property
            // set when things changed, which loading will do. But this doesn't
            // work until after parsing is complete.
            //mTempSet->SetRecordID( text.toInt() );
            mTempSet->SetUUID( m_ids.at( index ) );
            //mTempSet->SetUUID( text.toStdString() );
            mTempSet->LoadFromDatabase();
            mFeatureBrowser->RefreshAll();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
}// namespace conductor
}// namespace ves
