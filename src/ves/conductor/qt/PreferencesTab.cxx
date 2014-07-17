/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/conductor/qt/PreferencesTab.h>
#include <ves/conductor/qt/ui_PreferencesTab.h>

#include <ves/xplorer/data/PreferencesPropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <switchwire/OptionalMacros.h>

#include <iostream>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
PreferencesTab::PreferencesTab( QWidget* parent ) :
    QWidget( parent ),
    ui( new Ui::PreferencesTab )
{
    ui->setupUi( this );

    QPushButton *grab_button = qFindChild< QPushButton* >( this, "btnGrabCurrentCameraState" );
    connect( grab_button, SIGNAL( clicked() ), this, SLOT( onGrabCurrentCameraState() ) );

    CONNECTSIGNAL_0( "VesFileLoaded",
                     void( std::string const & ),
                     &PreferencesTab::reloadPreferencesPropertySet,
                     m_connections, normal_Priority );

    m_propertySet = propertystore::PropertySetPtr( new ves::xplorer::data::PreferencesPropertySet() );

    m_propertySet->Save();

    ui->preferencesPropertyBrowser->ParsePropertySet( m_propertySet );
    ui->preferencesPropertyBrowser->show();
}
////////////////////////////////////////////////////////////////////////////////
PreferencesTab::~PreferencesTab()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesTab::changeEvent( QEvent* e )
{
    QWidget::changeEvent( e );
    switch( e->type() )
    {
    case QEvent::LanguageChange:
        ui->retranslateUi( this );
        break;
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesTab::onGrabCurrentCameraState()
{
    osgwMx::MxCore& viewmat =  ves::xplorer::scenegraph::SceneManager::instance()->GetMxCoreViewMatrix();
    osg::Vec3d view = viewmat.getDir();
    osg::Vec3d pos = viewmat.getPosition();

    m_propertySet->GetProperty( "Camera_ViewDirection_X" )->SetValue( view[0] );
    m_propertySet->GetProperty( "Camera_ViewDirection_Y" )->SetValue( view[1] );
    m_propertySet->GetProperty( "Camera_ViewDirection_Z" )->SetValue( view[2] );
    m_propertySet->GetProperty( "Camera_Position_X" )->SetValue( pos[0] );
    m_propertySet->GetProperty( "Camera_Position_Y" )->SetValue( pos[1] );
    m_propertySet->GetProperty( "Camera_Position_Z" )->SetValue( pos[2] );

    ui->preferencesPropertyBrowser->RefreshValues();
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesTab::reloadPreferencesPropertySet()
{
    ves::xplorer::data::DatabaseManager* db_manager = ves::xplorer::data::DatabaseManager::instance();

    propertystore::PropertySetPtr prefs( new ves::xplorer::data::PreferencesPropertySet() );

    if( db_manager->TableExists( "XplorerPreferences" ) )
    {
        std::vector< std::string > uuids = db_manager->GetStringVector( "XplorerPreferences", "uuid" );

        prefs->LoadByKey( "uuid", uuids.back() );
    }

    m_propertySet = prefs;
    m_propertySet->Save();

    ui->preferencesPropertyBrowser->ParsePropertySet( m_propertySet );
    ui->preferencesPropertyBrowser->RefreshValues();
}

} // namespace conductor
} // namespace ves
