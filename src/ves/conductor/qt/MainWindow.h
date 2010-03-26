#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QMainWindow>

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    MainWindow(QWidget* parent = 0);
    ~MainWindow();

protected:
    void changeEvent(QEvent* e);
    void paintEvent ( QPaintEvent* event );

private:
    Ui::MainWindow* ui;
};

#endif // MAINWINDOW_H
