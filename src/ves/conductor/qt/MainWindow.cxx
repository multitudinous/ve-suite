#define QT_NO_KEYWORDS

#include "MainWindow.h"
#include "ui_MainWindow.h"
#include <QtGui/QMdiSubWindow>

#include <ves/conductor/qt/propertyBrowser/Visualization.h>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->mainToolBar->addAction(ui->action_New);
    ui->mainToolBar->addAction(ui->action_Open);
    ui->mainToolBar->addAction(ui->action_Save);

    // Make the background of this window translucent since we want to be able
    // to see through the empty parts of the mdi area.
    this->setAttribute(Qt::WA_TranslucentBackground);

    ui->mdiArea->setMouseTracking( true );
    
    ves::conductor::Visualization *visWindow = new ves::conductor::Visualization( this );
    QMdiSubWindow *subWin = ui->mdiArea->addSubWindow( visWindow );
    subWin->resize( 590, 489 );
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::changeEvent(QEvent *e)
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
