package ABS.DC;
// abslang.abs:622:0: 
public abstract class Resourcetype extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isCPU() { return this instanceof Resourcetype_CPU; }
    public final Resourcetype_CPU toCPU() { return (Resourcetype_CPU) this; }
    public final boolean isBandwidth() { return this instanceof Resourcetype_Bandwidth; }
    public final Resourcetype_Bandwidth toBandwidth() { return (Resourcetype_Bandwidth) this; }
    public final boolean isMemory() { return this instanceof Resourcetype_Memory; }
    public final Resourcetype_Memory toMemory() { return (Resourcetype_Memory) this; }
    public final boolean isStartupduration() { return this instanceof Resourcetype_Startupduration; }
    public final Resourcetype_Startupduration toStartupduration() { return (Resourcetype_Startupduration) this; }
    public final boolean isPaymentInterval() { return this instanceof Resourcetype_PaymentInterval; }
    public final Resourcetype_PaymentInterval toPaymentInterval() { return (Resourcetype_PaymentInterval) this; }
    public final boolean isCostPerInterval() { return this instanceof Resourcetype_CostPerInterval; }
    public final Resourcetype_CostPerInterval toCostPerInterval() { return (Resourcetype_CostPerInterval) this; }
}
