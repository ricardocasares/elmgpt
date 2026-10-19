// <elmcn-dialog>, the thin custom element behind UI.Dialog, UI.AlertDialog and
// UI.Drawer. Import it once from your JavaScript entry point:
//
//     import "./UI/elmcn-dialog";


export class ElmcnDialog extends HTMLElement {
  #ready = false;
  static observedAttributes = ["open"];

  connectedCallback() {
    this.#connected();
    this.#ready = true;
  }

  attributeChangedCallback() {
    if (this.#ready) {
      this.#connected()
    }
  }

  #connected() {
    const dialog = this.querySelector('dialog');
    const open = this.getAttribute('open') !== null;
    // A non-modal dialog is a popover: in the top layer, with the page behind it usable.
    const popover = dialog?.hasAttribute('popover') ?? false;
    const shown = popover ? dialog?.matches(':popover-open') : dialog?.open;

    if (dialog && open && !shown) {
      if (popover) dialog.showPopover();
      else dialog.showModal();
    } else if (dialog && shown) {
      // Close once the exit transition ends, unless the dialog was opened again
      // meanwhile. Only the dialog's own children count, the overlay and the
      // panel: a button or input inside also transitions when a click moves
      // focus away from it, and its `transitionend` bubbles up here first.
      const onTransitionEnd = (event: TransitionEvent) => {
        if (!(event.target instanceof Element) || event.target.parentElement !== dialog) return;
        dialog.removeEventListener('transitionend', onTransitionEnd);
        if (this.getAttribute('open') !== null) return;
        if (popover) dialog.hidePopover();
        else dialog.close();
      };

      dialog.addEventListener('transitionend', onTransitionEnd);
    }
  }
}

if (!customElements.get("elmcn-dialog")) customElements.define("elmcn-dialog", ElmcnDialog);
