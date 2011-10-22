#include "LauncherMainWindow.h"
#include "ui_LauncherMainWindow.h"
#include <QtGui/QDesktopWidget>
#include <QtGui/QFileDialog>
#include <QtCore/QSettings>

LauncherMainWindow::LauncherMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::LauncherMainWindow)
{
    ui->setupUi(this);
    QDesktopWidget* dw = QApplication::desktop();
    const QRect geom = dw->screenGeometry( dw->screenNumber( this ) );
    ui->m_width->setValue( geom.width() );
    ui->m_height->setValue( geom.height() );

    QSettings settings( "VESuite.org", "VELauncher" );

    bool notFirstRun = settings.value( "launcher/notFirstRun" ).toBool();

    if( notFirstRun )
    {
        ui->m_workingDir->setText( settings.value( "xplorer/workingDir" ).toString() );
        ui->m_configuration->setText( settings.value( "xplorer/jconf" ).toString() );
        ui->m_desktopMode->setChecked( settings.value( "xplorer/desktop" ).toBool() );

        int xplorerWidth = settings.value( "xplorer/width" ).toInt();
        int xplorerHeight = settings.value( "xplorer/height" ).toInt();

        if( (xplorerWidth != 0) && (xplorerHeight != 0) )
        {
            ui->m_width->setValue( xplorerWidth );
            ui->m_height->setValue( xplorerHeight );
        }

        ui->m_RTT->setChecked( settings.value( "xplorer/RTT" ).toBool() );
        ui->m_logLevel->setCurrentIndex( settings.value( "xplorer/logLevel" ).toInt() );
        ui->m_showStdout->setChecked( settings.value( "xplorer/showStdout" ).toBool() );

        int width = settings.value( "launcher/width" ).toInt();
        int height = settings.value( "launcher/height").toInt();

        if( width != 0 && height != 0 )
        {
            this->resize( settings.value( "launcher/width" ).toInt(),
                          settings.value( "launcher/height").toInt() );
        }
        this->move( settings.value( "launcher/xposition").toInt(),
                    settings.value( "launcher/yposition").toInt() );
    }
}

LauncherMainWindow::~LauncherMainWindow()
{
    delete ui;
}

void LauncherMainWindow::changeEvent(QEvent *e)
{
    QMainWindow::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

void LauncherMainWindow::on_launch_clicked()
{
    QString program = "ves_xplorer";
    QStringList arguments;

    QString jconf = ui->m_configuration->text();
    arguments << "--jconf" <<  jconf;

    if( ui->m_desktopMode->isChecked() )
    {
        QString width;
        width.setNum( ui->m_width->value() );
        QString height;
        height.setNum( ui->m_height->value() );
        arguments << "--VESDesktop" << width <<  "--VESDesktop" << height;
    }

    if( ui->m_RTT->isChecked() )
    {
        arguments <<  "--VESRTT";
    }

    arguments << "--VESLog" << ui->m_logLevel->currentText();

    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    m_process = new QProcess( 0 );
    m_process->setProcessEnvironment( env );
    m_process->start(program, arguments);

    // Open a textedit window and use it to display contents of stdoutput from
    // xplorer
    if( ui->m_showStdout->isChecked() )
    {
        m_stdout = new QTextEdit( 0 );
        m_stdout->resize( 640, 360 );
        m_stdout->show();
        m_stdout->setWindowTitle( "VE-Suite stdoutput" );
        connect( m_process, SIGNAL(readyReadStandardOutput()), this, SLOT(onReadyReadStandardOutput()) );
    }

    QSettings settings( "VESuite.org", "VELauncher" );
    settings.setValue( "xplorer/workingDir", QDir::currentPath() );
    settings.setValue( "xplorer/jconf", jconf );
    settings.setValue( "xplorer/desktop", ui->m_desktopMode->isChecked() );
    settings.setValue( "xplorer/width", ui->m_width->value() );
    settings.setValue( "xplorer/height", ui->m_height->value() );
    settings.setValue( "xplorer/RTT", ui->m_RTT->isChecked() );
    settings.setValue( "xplorer/logLevel", ui->m_logLevel->currentIndex() );
    settings.setValue( "xplorer/showStdout", ui->m_showStdout->isChecked() );
    settings.setValue( "launcher/notFirstRun", true );
    settings.setValue( "launcher/width", this->width() );
    settings.setValue( "launcher/height", this->height() );
    settings.setValue( "launcher/xposition", this->x() );
    settings.setValue( "launcher/yposition", this->y() );

    this->close();
}

void LauncherMainWindow::on_m_workingDirButton_clicked()
{
    // Get the new working dir and change current path to it
    QString dir = QFileDialog::getExistingDirectory(this, tr("Select Working Directory"),
                                                     "",
                                                     QFileDialog::ShowDirsOnly
                                                     | QFileDialog::DontResolveSymlinks);

    if( !dir.isEmpty() )
    {
        ui->m_workingDir->setText( dir );
        QDir::setCurrent( dir );
    }
}

void LauncherMainWindow::on_m_configurationButton_clicked()
{
    QString fileName = QFileDialog::getOpenFileName(this, tr("Select configuration file"),
                                                     "",
                                                     tr("Juggler configuration (*.jconf)"));

    if( !fileName.isEmpty() )
    {
        ui->m_configuration->setText( fileName );
    }
}

void LauncherMainWindow::onReadyReadStandardOutput()
{
    QByteArray data = m_process->readAllStandardOutput();
    QString stringData( data );
    m_stdout->append( stringData );
}
