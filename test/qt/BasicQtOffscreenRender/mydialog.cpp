#include "mydialog.h"
#include "ui_mydialog.h"

myDialog::myDialog(QWidget *parent)
    : QDialog(parent), ui(new Ui::myDialog)
{
    ui->setupUi(this);
}

myDialog::~myDialog()
{
    delete ui;
}

 void myDialog::mouseMoveEvent( QMouseEvent *event )
 {
     //event->accept();
     /*
     QString text;
     text.setNum( event->x() );
     this->findChild<QLabel *>( "label_x" )->setText( text );
     text.setNum( event->y() );
     this->findChild<QLabel *>( "label_y" )->setText( text );
     */
     this->findChild<QLabel *>( "dialogX" )->setNum( event->x() );
     this->findChild<QLabel *>( "dialogY" )->setNum( event->y() );
 }
