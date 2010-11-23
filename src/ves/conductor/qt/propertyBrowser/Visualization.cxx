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

#include <boost/concept_check.hpp>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
Visualization::Visualization( QWidget* parent ) 
    :
    QDialog( parent ),
    m_ui( new Ui::Visualization )
{
    m_ui->setupUi( this );

    mFeatureBrowser = new PropertyBrowser( this );

    // Force FeatureIDSelector to update itself based on the database.
    UpdateFeatureIDSelectorChoices();
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
        std::string featureName = m_ui->FeaturesList->currentItem()->text().toStdString();
        VisFeatureManager::instance()->UpdateFeature( featureName, mTempSet->GetRecordID() );
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
    QString featureName = m_ui->FeaturesList->currentItem()->text();
    mTempSet = VisFeatureManager::instance()->
            CreateNewFeature( featureName.toStdString() );

    if( mTempSet )
    {
        mTempSet->WriteToDatabase();
    }

    mFeatureBrowser->ParsePropertySet( mTempSet );

    // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
    // to take the Qt-ized data from a PropertyBrowser such as
    // mFeatureBrowser and display it in the GUI.
    m_ui->vfpb->setPropertyBrowser( mFeatureBrowser );
    m_ui->vfpb->RefreshContents();
    m_ui->vfpb->show();

    // Re-read available feature IDs from database, and select the last one added,
    // which by definition corresponds to the "new" one
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
    // Remove all entries from feature id selection combobox
    m_ui->FeatureIDSelector->clear();

    // Get all available IDs for current feature type
    QString featureName = m_ui->FeaturesList->currentItem()->text();
    std::vector<std::string> ids = VisFeatureManager::instance()->GetIDsForFeature( featureName.toStdString() );

    // Convert IDs to format needed by QComboBox and add them as choices
    QStringList idList;
    QString qCurrentID;
    for( size_t index = 0; index < ids.size(); index++ )
    {
        qCurrentID = qCurrentID.fromStdString( ids.at( index ) );
        idList.append( qCurrentID );
    }

    m_ui->FeatureIDSelector->addItems( idList );

    m_ui->FeatureIDSelector->setCurrentIndex( m_ui->FeatureIDSelector->count() - 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Visualization::on_FeatureIDSelector_currentIndexChanged( const QString& text )
{
    ves::xplorer::data::PropertySetPtr nullPtr;

    if( text.isEmpty() )
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
            mTempSet->SetRecordID( text.toInt() );
            mTempSet->LoadFromDatabase();
            mFeatureBrowser->ParsePropertySet( mTempSet );

            // ui.vfpb is an instance of GenericPropertyBrowser, which knows how
            // to take the Qt-ized data from a PropertyBrowser such as
            // mFeatureBrowser and display it in the GUI.
            m_ui->vfpb->setPropertyBrowser( mFeatureBrowser );
            m_ui->vfpb->RefreshContents();
            m_ui->vfpb->show();
            mTempSet->LoadFromDatabase();
            mFeatureBrowser->RefreshAll();
        }
    }

}
////////////////////////////////////////////////////////////////////////////////
}// namespace conductor
}// namespace ves
