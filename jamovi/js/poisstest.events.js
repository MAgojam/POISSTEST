 
const events = {
    onChange_switch: function(ui) {

        if (ui.switchr.value()){
            ui.spanel.collapse();
            ui.rpanel.expand();
        } else {
            ui.spanel.expand();
            ui.rpanel.collapse();
        }
    }
};

module.exports = events;
