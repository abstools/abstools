package abs.backend.java.observing;

public interface GuardView {
    boolean isTrue();

    boolean isExpressionGuard();

    boolean isFutureGuard();

    boolean isAndGuard();

    GuardView getLeftGuard();

    GuardView getRightGuard();

    FutView getFuture();

    String toABSString();
}
