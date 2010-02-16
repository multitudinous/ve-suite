#ifndef MYDIALOG_H
#define MYDIALOG_H

#include <QtGui/QDialog>
#include <QtGui/QMouseEvent>

namespace Ui
{
    class myDialog;
}

class myDialog : public QDialog
{
    Q_OBJECT

public:
    myDialog(QWidget *parent = 0);
    ~myDialog();

    void mouseMoveEvent( QMouseEvent *event );

private:
    Ui::myDialog *ui;
};

#endif // MYDIALOG_H
