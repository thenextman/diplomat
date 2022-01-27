// Automatically generated by Diplomat

#pragma warning disable 0105
using System;
using System.Runtime.InteropServices;

using DiplomatFeatures.Diplomat;
#pragma warning restore 0105

namespace DiplomatFeatures;

#nullable enable

public partial class Opaque: IDisposable
{
    private unsafe Raw.Opaque* _inner;

    /// <summary>
    /// Creates a managed <c>Opaque</c> from a raw handle.
    /// </summary>
    /// <remarks>
    /// Safety: you should not build two managed objects using the same raw handle (may causes use-after-free and double-free).
    /// </remarks>
    /// <remarks>
    /// This constructor assumes the raw struct is allocated on Rust side.
    /// If implemented, the custom Drop implementation on Rust side WILL run on destruction.
    /// </remarks>
    public unsafe Opaque(Raw.Opaque* handle)
    {
        _inner = handle;
    }

    /// <returns>
    /// A <c>Opaque</c> allocated on Rust side.
    /// If a custom Drop implementation is implemented on Rust side, it WILL run on destruction.
    /// </returns>
    public static Opaque New()
    {
        unsafe
        {
            Raw.Opaque* retVal = Raw.Opaque.New();
            return new Opaque(retVal);
        }
    }

    public void AssertStruct(MyStruct s)
    {
        unsafe
        {
            if (_inner == null)
            {
                throw new ObjectDisposedException("Opaque");
            }
            Raw.MyStruct* sRaw;
            sRaw = s.AsFFI();
            if (sRaw == null)
            {
                throw new ObjectDisposedException("MyStruct");
            }
            Raw.Opaque.AssertStruct(_inner, *sRaw);
        }
    }

    /// <summary>
    /// Returns the underlying raw handle.
    /// </summary>
    public unsafe Raw.Opaque* AsFFI()
    {
        return _inner;
    }

    /// <summary>
    /// Marks this object as moved into Rust side.
    /// </summary>
    public void MarkAsMoved()
    {
        unsafe
        {
            if (_inner == null)
            {
                throw new ObjectDisposedException("Opaque");
            }
            _inner = null;
        }
    }

    /// <summary>
    /// Restores unmanaged ressource handle to this object.
    /// </summary>
    public unsafe void RestoreHandle(Raw.Opaque* handle)
    {
        _inner = handle;
    }

    /// <summary>
    /// Destroys the underlying object immediately.
    /// </summary>
    public void Dispose()
    {
        unsafe
        {
            if (_inner == null)
            {
                return;
            }

            Raw.Opaque.Destroy(_inner);
            _inner = null;

            GC.SuppressFinalize(this);
        }
    }

    ~Opaque()
    {
        Dispose();
    }
}
