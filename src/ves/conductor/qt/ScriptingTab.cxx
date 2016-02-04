#include <ves/conductor/qt/ScriptingTab.h>
#include <ves/conductor/qt/ui_ScriptingTab.h>
#include <ves/conductor/qt/UITabs.h>

#include <QtGui/QFileDialog>
#include <QtCore/QFile>
#include <QtCore/QTextStream>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <switchwire/Version.h>

#include <ves/conductor/qt/scriptingTools/SquirrelConnection.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/PartManipulatorPropertySet.h>

#include <vpr/Thread/Thread.h>
#include <vpr/System.h>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
ScriptingTab::ScriptingTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ScriptingTab),
    m_currentLoadButton(0),
    m_fileDialog(0),
    m_partManipThread(0)
{
    ui->setupUi(this);

    m_loadButtonVector.push_back( ui->m_load_1 );
    m_loadButtonVector.push_back( ui->m_load_2 );
    m_loadButtonVector.push_back( ui->m_load_3 );
    m_loadButtonVector.push_back( ui->m_load_4 );
    m_loadButtonVector.push_back( ui->m_load_5 );
    m_loadButtonVector.push_back( ui->m_load_6 );
    m_loadButtonVector.push_back( ui->m_load_7 );
    m_loadButtonVector.push_back( ui->m_load_8 );
    m_loadButtonVector.push_back( ui->m_load_9 );
    m_loadButtonVector.push_back( ui->m_load_10 );
    m_loadButtonVector.push_back( ui->m_load_11 );
    m_loadButtonVector.push_back( ui->m_load_12 );

    //Pair a load button with a run button
    m_buttonMap[ ui->m_load_1 ] = ui->m_run_1;
    m_buttonMap[ ui->m_load_2 ] = ui->m_run_2;
    m_buttonMap[ ui->m_load_3 ] = ui->m_run_3;
    m_buttonMap[ ui->m_load_4 ] = ui->m_run_4;
    m_buttonMap[ ui->m_load_5 ] = ui->m_run_5;
    m_buttonMap[ ui->m_load_6 ] = ui->m_run_6;
    m_buttonMap[ ui->m_load_7 ] = ui->m_run_7;
    m_buttonMap[ ui->m_load_8 ] = ui->m_run_8;
    m_buttonMap[ ui->m_load_9 ] = ui->m_run_9;
    m_buttonMap[ ui->m_load_10 ] = ui->m_run_10;
    m_buttonMap[ ui->m_load_11 ] = ui->m_run_11;
    m_buttonMap[ ui->m_load_12 ] = ui->m_run_12;

    //Click signal connections for all the load and run buttons on the multi tab
    std::map< QObject*, QObject* >::iterator it = m_buttonMap.begin();
    while( it != m_buttonMap.end() )
    {
        connect( it->first, SIGNAL(clicked()),
                 this, SLOT(multi_load_clicked()) );
        connect( it->second, SIGNAL(clicked()),
                 this, SLOT(multi_run_clicked()) );
        ++it;
    }

    connect( this, SIGNAL(SetButtonTextSignal(QPushButton*, const QString&)),
             this, SLOT(DelayedSetButtonText(QPushButton*, const QString&)),
             Qt::QueuedConnection );

    CONNECTSIGNALS_2( "%AssociateScript", void( const std::string&, int ),
                      &ScriptingTab::AssociateScript,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNAL_0( "VesFileLoaded",
                     void( std::string const & ),
                     &ScriptingTab::ApplyPartManipulatorPropertySets,
                     m_connections, normal_Priority );

    CONNECTSIGNALS_1( "%CADSelection",
                      void( bool const& ),
                      &ScriptingTab::HandleCADSelection,
                      m_connections, any_SignalType, normal_Priority );

    switchwire::EventManager::instance()->RegisterSignal(
        &m_destroySignal,
        "ScriptingTab.Destroy" );

    StartPartManipulatorScript();
}
////////////////////////////////////////////////////////////////////////////////
ScriptingTab::~ScriptingTab()
{
    StopPartManipulatorScript();

    delete ui;
    for( size_t idx = 0; idx < m_threads.size(); ++idx )
    {
        m_threads.at( idx )->join();
        delete m_threads.at( idx );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::on_m_openButton_clicked()
{
    //QFileDialog* fd = new QFileDialog(0);
    m_fileDialog = new QFileDialog(0);
    m_fileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    m_fileDialog->setAttribute( Qt::WA_DeleteOnClose );
    m_fileDialog->setFileMode( QFileDialog::ExistingFiles );
    m_fileDialog->setDirectory( QDir::current() );
    QStringList filters;
    filters << "Squirrel Scripts (*.nut)";
    m_fileDialog->setNameFilters( filters );

    QObject::connect( m_fileDialog, SIGNAL( filesSelected( const QStringList& ) ),
                      this, SLOT( onFileOpenSelected( const QStringList& ) ) );

    int index = ves::conductor::UITabs::instance()->AddTab( m_fileDialog, "Open Script", true );
    ves::conductor::UITabs::instance()->ActivateTab( index );
    //m_fileDialog->show();
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::onFileOpenSelected( const QStringList& files )
{
    // m_fileDialog was given mode DeleteOnClose, so we can just set to null.
    m_fileDialog = 0;
    if( !files.isEmpty() )
    {
        ui->m_scriptPath->setText( files.at( 0 ) );
    }

    QFile file( files.at( 0 ) );
    if (file.open(QFile::ReadOnly | QFile::Text))
    {
        ui->m_scriptEdit->setPlainText( file.readAll() );
    }

}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::on_m_runButton_clicked()
{
    if ( ui->m_scriptEdit->toPlainText().toStdString().empty() )
    {
        return;
    }

    vpr::Thread* thread =
            new vpr::Thread( boost::bind(&ScriptingTab::runScript, this, ui->m_scriptEdit->toPlainText().toStdString()) );
    m_threads.push_back( thread );
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::runScript( const std::string& scriptText )
{
#if SWITCHWIRE_HAVE_SQUIRREL
    ves::conductor::SquirrelConnection sc( scriptText );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::multi_load_clicked()
{
    m_currentLoadButton = QObject::sender();

    //QFileDialog* fd = new QFileDialog(0);
    m_fileDialog = new QFileDialog(0);
    m_fileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    m_fileDialog->setAttribute( Qt::WA_DeleteOnClose );
    m_fileDialog->setFileMode( QFileDialog::ExistingFiles );
    m_fileDialog->setDirectory( QDir::current() );
    QStringList filters;
    filters << "Squirrel Scripts (*.nut)";
    m_fileDialog->setNameFilters( filters );

    QObject::connect( m_fileDialog, SIGNAL( filesSelected( const QStringList& ) ),
                      this, SLOT( MultiOpenSelected( const QStringList& ) ) );

    int index = ves::conductor::UITabs::instance()->AddTab( m_fileDialog, "Open Script", true );
    ves::conductor::UITabs::instance()->ActivateTab( index );
    //fd->show();
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::MultiOpenSelected( const QStringList& files )
{
    // m_fileDialog was given mode DeleteOnClose, so we can just set to null.
    m_fileDialog = 0;

    QString fileName = files.at( 0 ).split( QRegExp( "[\\/]" ) ).last();
    dynamic_cast< QPushButton* >(m_buttonMap[ m_currentLoadButton ])->setText( fileName );
    m_scriptMap[ m_buttonMap[ m_currentLoadButton ] ] = files.at( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::multi_run_clicked()
{
    QFile file( m_scriptMap[ QObject::sender() ] );
    if (file.open(QFile::ReadOnly | QFile::Text))
    {
        QString script = file.readAll();
        vpr::Thread* thread =
                new vpr::Thread( boost::bind(&ScriptingTab::runScript,
                                             this, script.toStdString() ) );
        m_threads.push_back( thread );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::AssociateScript( const std::string& scriptPath,
                                    int buttonIndex )
{
    if( static_cast<size_t>(buttonIndex) < m_loadButtonVector.size() )
    {
        QString qScriptPath = QString::fromStdString( scriptPath );
        QString fileName = qScriptPath.split( QRegExp( "[\\/]" ) ).last();
        SetButtonTextSignal( dynamic_cast< QPushButton* >(
                             m_buttonMap[m_loadButtonVector.at( buttonIndex )] ), fileName );
        m_scriptMap[ m_buttonMap[ m_loadButtonVector.at( buttonIndex ) ] ] =
                                                                    qScriptPath;
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::DelayedSetButtonText( QPushButton* button, const QString& text )
{
    button->setText( text );
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::ApplyPartManipulatorPropertySets()
{
    ves::xplorer::data::DatabaseManager* db_manager = ves::xplorer::data::DatabaseManager::instance();

    if( db_manager->TableExists( "PartManipulatorPropertySet" ) )
    {
        std::vector< std::string > paths = db_manager->GetStringVector( "PartManipulatorPropertySet", "NodePath" );

        std::vector< std::string >::const_iterator i;

        for( i = paths.begin(); i != paths.end(); i++ )
        {
            propertystore::PropertySetPtr set( new ves::xplorer::data::PartManipulatorPropertySet() );
            static_cast< ves::xplorer::data::PartManipulatorPropertySet* >( set.get() )->InitializeWithNodePath( *i );
        }
    }
}

void ScriptingTab::StartPartManipulatorScript()
{
    if( !m_partManipThread )
    {
        std::string part_manip_path;
        vpr::System::getenv( "XPLORER_BASE_DIR", part_manip_path );
        part_manip_path += "/share/vesuite/squirrel/part_manipulator.nut";

        QFile part_manip_file( part_manip_path.c_str() );
        part_manip_file.open( QIODevice::ReadOnly | QIODevice::Text );
        QString s;
        QTextStream in( &part_manip_file );
        s.append( in.readAll() );

        m_partManipThread = new vpr::Thread( boost::bind( &ScriptingTab::runScript, this, s.toStdString() ) );
    }
}

void ScriptingTab::StopPartManipulatorScript()
{
    if( m_partManipThread )
    {
        m_destroySignal.signal( true );

        m_partManipThread->join();
        delete m_partManipThread;
        m_partManipThread = NULL;
    }
}

void ScriptingTab::HandleCADSelection( bool const& enabled )
{
    if( enabled )
    {
        std::cout << "Stopping part manipulator script..." << std::endl << std::flush;
        StopPartManipulatorScript();
    }
    else
    {
        std::cout << "Starting part manipulator script..." << std::endl << std::flush;
        StartPartManipulatorScript();
    }
}
}} // ves::conductor
