#include <ves/conductor/qt/ScriptingTab.h>
#include <ves/conductor/qt/ui_ScriptingTab.h>

#include <QtGui/QFileDialog>
#include <QtCore/QFile>

#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/conductor/qt/VisFeatureManager.h>

#include <switchwire/EventManager.h>
#include <switchwire/Version.h>
#if SWITCHWIRE_HAVE_SQUIRREL
#include <switchwire/squirrel/ConnectScripts.h>
#include <switchwire/squirrel/ExposeSignals.h>
#include <switchwire/squirrel/SQStdMap.h>
#include <switchwire/squirrel/SQStdVector.h>

#include <squirrel.h>
//DIAG_OFF(unused-parameter)
#include <sqrat.h>
#include <sqrat/sqratVM.h>
//DIAG_ON(unused-parameter)
#endif

namespace ves
{
namespace conductor
{

// This class will eventually move into some scripting utility library along
// with similar access classes for touching core VES pieces
class VizPropertySetWrapper
{
public:

    void CreateNewFeature( const std::string& featureType )
    {
        m_set = ves::conductor::VisFeatureManager::instance()->CreateNewFeature( featureType );
    }

    void SetPropertyValue( const std::string& key, boost::any value )
    {
        m_set->SetPropertyValue( key, value );
    }

    void SetDoublePropertyValue( const std::string& key, double value )
    {
        m_set->SetPropertyValue( key, value );
    }

    void SetStringPropertyValue( const std::string& key, std::string value )
    {
        m_set->SetPropertyValue( key, value );
    }

    std::string GetUUIDAsString()
    {
        return m_set->GetUUIDAsString();
    }

    void Save()
    {
        m_set->Save();
    }

private:
    propertystore::PropertySetPtr m_set;
};
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
ScriptingTab::ScriptingTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ScriptingTab)
{
    ui->setupUi(this);
}
////////////////////////////////////////////////////////////////////////////////
ScriptingTab::~ScriptingTab()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::on_m_openButton_clicked()
{
    QFileDialog* fd = new QFileDialog(0);
    fd->setOptions( QFileDialog::DontUseNativeDialog );
    // Make mFileDialog manage its own lifetime and memory
    fd->setAttribute( Qt::WA_DeleteOnClose );
    fd->setFileMode( QFileDialog::ExistingFiles );
    // Tell the dialog to open in the cwd
    fd->setDirectory( QDir::current() );
    QStringList filters;
    filters << "Squirrel Scripts (*.nut)";
    fd->setNameFilters( filters );

    QObject::connect( fd, SIGNAL( filesSelected( const QStringList& ) ),
                      this, SLOT( onFileOpenSelected( const QStringList& ) ) );

    fd->show();
}
////////////////////////////////////////////////////////////////////////////////
void ScriptingTab::onFileOpenSelected( const QStringList& files )
{
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
// NOTE: A lot of what is in this method will eventually move into other
// classes, but until the basic kinks are worked out of this scripting setup,
// it's easier to deal with it all in one place.
void ScriptingTab::on_m_runButton_clicked()
{
#if SWITCHWIRE_HAVE_SQUIRREL
    // Set up a default Squirrel VM.
    Sqrat::SqratVM vm;
    Sqrat::DefaultVM::Set(vm.getVM());

    // Expose a void( const std::string& ) signal type to the vm so
    // that scripts can register signals of that type. Our test will use that
    // type to load a file.
    ExposeSignalType_1< void( const std::string& ) >( "VoidStdStringConstRefSignal", vm );
    // Expose the signal type required for toggling geometry and viz features
    ExposeSignalType_2< void( const std::string&, const bool& ) >( "v_sscr_bcr_signal", vm );

    typedef VizPropertySetWrapper psw;
    Sqrat::Class< psw > psClass;
    psClass.Func( "CreateNewFeature", &psw::CreateNewFeature );
    psClass.Func( "SetPropertyValue", &psw::SetPropertyValue );
    psClass.Func( "SetDoublePropertyValue", &psw::SetDoublePropertyValue );
    psClass.Func( "SetStringPropertyValue", &psw::SetStringPropertyValue );
    psClass.Func( "Save", &psw::Save );
    psClass.Func( "GetUUIDAsString", &psw::GetUUIDAsString );
    Sqrat::RootTable().Bind( "VizPropertySet", psClass );

    std::string scriptPath = ui->m_scriptPath->text().toStdString();

    try
    {
        Sqrat::Script script;
        script.CompileString( ui->m_scriptEdit->toPlainText().toStdString() );
        //script.CompileFile( scriptPath );
        script.Run();
    }
    catch( Sqrat::Exception& e )
    {
        std::cout << "Sqrat exception: " << e.Message() << std::endl << std::flush;
        return;
    }
    catch( ... )
    {
        std::cout << "Unspecified Sqrat exception" << std::endl << std::flush;
        return;
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
}} // ves::conductor
