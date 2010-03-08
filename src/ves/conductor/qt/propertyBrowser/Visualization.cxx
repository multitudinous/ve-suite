#define QT_NO_KEYWORDS
// THIS HEADER MUST COME FIRST
// Header dynamically generated during build process by uic. DO NOT INSERT PATH
// INFORMATION!
#include "ui_Visualization.h"

#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>

#include <iostream>

using namespace ves::conductor;

Visualization::Visualization( QWidget *parent ) :
QDialog( parent ),
ui( new Ui::Visualization )
{
    ui->setupUi( this );

    mFeatureBrowser = new PropertyBrowser( this );
}

Visualization::~Visualization( )
{
    if( ui )
    {
        delete ui;
    }

    if( mFeatureBrowser )
    {
        delete mFeatureBrowser;
    }
    
    if( mTempSet)
    {
        delete mTempSet;
    }
}

void Visualization::changeEvent( QEvent *e )
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
        mTempSet->WriteToDatabase( "/home/penn/vesTest.db" );
    }
}

void Visualization::on_RefreshPropertiesButton_clicked( )
{
    if( mTempSet )
    {
        mTempSet->LoadFromDatabase( "/home/penn/vesTest.db" );
        mFeatureBrowser->RefreshAll();
    }
}

void Visualization::on_NewFeatureButton_clicked( )
{
    mTempSet = new xplorer::data::ContourPlanePropertySet( );
    if( mTempSet )
    {
        mTempSet->WriteToDatabase( "/home/penn/vesTest.db" );
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
        mTempSet->DeleteFromDatabase( "/home/penn/vesTest.db" );
        mFeatureBrowser->ParsePropertySet( NULL );
        delete mTempSet;
    }
}
