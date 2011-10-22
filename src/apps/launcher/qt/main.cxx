#include <QtGui/QApplication>
#include "LauncherMainWindow.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    LauncherMainWindow w;
    w.show();

    return a.exec();
}
