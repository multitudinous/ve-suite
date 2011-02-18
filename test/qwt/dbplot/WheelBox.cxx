
// --- DB Plot Includes --- //
#include "WheelBox.h"

// --- QT Includes --- //
#include <QtGui/qlcdnumber.h>
#include <QtGui/qlabel.h>
#include <QtGui/qlayout.h>
#include <QtGui/qevent.h>
#include <QtGui/qapplication.h>

// --- QWT Includes --- //
#include <qwt_wheel.h>

////////////////////////////////////////////////////////////////////////////////
class Wheel : public QwtWheel
{
public:
    ///
    Wheel( WheelBox* parent )
        :
        QwtWheel( parent )
    {
        setFocusPolicy( Qt::WheelFocus );
        parent->installEventFilter( this );
    }

    ///
    virtual bool eventFilter( QObject* object, QEvent* ev )
    {
        if( ev->type() == QEvent::Wheel )
        {
            QWheelEvent* we = (QWheelEvent*)( ev );

            QWheelEvent wheelEvent(
                QPoint( 5, 5 ),
                we->delta(),
                we->buttons(),
                we->modifiers(),
                we->orientation() );

            QApplication::sendEvent( this, &wheelEvent );
            return true;
        }

        return QwtWheel::eventFilter( object, ev );
    }

};
////////////////////////////////////////////////////////////////////////////////
WheelBox::WheelBox(
    QString const& title,
    double min,
    double max,
    double stepSize,
    QWidget* parent )
    :
    QWidget( parent )
{

    m_number = new QLCDNumber( this );
    m_number->setSegmentStyle( QLCDNumber::Filled );
    m_number->setAutoFillBackground( true );
    m_number->setFixedHeight( m_number->sizeHint().height() * 2 );
    m_number->setFocusPolicy( Qt::WheelFocus );

    QPalette pal( Qt::black );
    pal.setColor( QPalette::WindowText, Qt::green );
    m_number->setPalette( pal );

    m_wheel = new Wheel( this );
    m_wheel->setOrientation( Qt::Vertical );
    m_wheel->setRange( min, max, stepSize );
    m_wheel->setFixedSize(
        qRound( m_number->height() / 2.5 ), m_number->height() );

    m_number->setFocusProxy( m_wheel );

    QFont font( "Helvetica", 10 );
    font.setBold( true );

    m_label = new QLabel( title, this );
    m_label->setFont( font );

    QHBoxLayout* hLayout = new QHBoxLayout();
    hLayout->setContentsMargins( 0, 0, 0, 0 );
    hLayout->setSpacing( 2 );
    hLayout->addWidget( m_number, 10 );
    hLayout->addWidget( m_wheel );

    QVBoxLayout* vLayout = new QVBoxLayout( this );
    vLayout->addLayout( hLayout, 10 );
    vLayout->addWidget( m_label, 0, Qt::AlignTop | Qt::AlignHCenter );

    connect(
        m_wheel, SIGNAL( valueChanged( double ) ),
        m_number, SLOT( display( double ) ) );
    connect(
        m_wheel, SIGNAL( valueChanged( double ) ),
        this, SIGNAL( valueChanged( double ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void WheelBox::setValue( double value )
{
    m_wheel->setValue( value );
    m_number->display( value );
}
////////////////////////////////////////////////////////////////////////////////
double WheelBox::value() const
{
    return m_wheel->value();
}
////////////////////////////////////////////////////////////////////////////////
