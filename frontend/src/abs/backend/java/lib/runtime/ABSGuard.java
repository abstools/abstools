package abs.backend.java.lib.runtime;

import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;

public abstract class ABSGuard {
	public abstract boolean isTrue();

    public void await() { }

    private GuardView view;
    public GuardView getView() {
        if (view == null)
            view = new View();
        return view;
    }
    
    private class View implements GuardView {

        @Override
        public boolean isTrue() {
            return ABSGuard.this.isTrue();
        }

        @Override
        public boolean isExpressionGuard() {
            return ABSGuard.this instanceof ABSExpGuard;
        }

        @Override
        public boolean isFutureGuard() {
            return ABSGuard.this instanceof ABSFutureGuard;
        }

        @Override
        public boolean isAndGuard() {
            return ABSGuard.this instanceof ABSAndGuard;
        }

        @Override
        public GuardView getLeftGuard() {
            return ((ABSAndGuard)ABSGuard.this).getLeftGuard().getView();
        }

        @Override
        public GuardView getRightGuard() {
            return ((ABSAndGuard)ABSGuard.this).getRightGuard().getView();
        }

        @Override
        public FutView getFuture() {
            return ((ABSFutureGuard)ABSGuard.this).fut.getView();
        }
        
    }
}
