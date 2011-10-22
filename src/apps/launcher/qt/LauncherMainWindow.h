#ifndef LAUNCHERMAINWINDOW_H
#define LAUNCHERMAINWINDOW_H

#include <QtGui/QMainWindow>
#include <QtGui/QTextEdit>
#include <QtCore/QProcess>

namespace Ui {
    class LauncherMainWindow;
}

class LauncherMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit LauncherMainWindow(QWidget *parent = 0);
    ~LauncherMainWindow();

protected:
    void changeEvent(QEvent *e);

private:
    Ui::LauncherMainWindow *ui;
    QTextEdit* m_stdout;
    QProcess* m_process;

private slots:
    void on_launch_clicked();
    void on_m_workingDirButton_clicked();
    void on_m_configurationButton_clicked();
    void onReadyReadStandardOutput();
};

#endif // LAUNCHERMAINWINDOW_H
