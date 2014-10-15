$(document).ready(function() {
    var websocket;

    var MessageType = {
        INFO : 0,
        ERROR : 1,
        REQUEST : 2,
        RESPONSE : 3
    };

    var ReadyState = {
        CONNECTING : 0,
        OPEN : 1,
        CLOSING : 2,
        CLOSED : 3
    };

    testPrerequisites();
    initElements();

    function testPrerequisites() {
        var hasWS = ("WebSocket" in window);
        $('#checkWs').removeClass('undetermined')
                     .addClass(hasWS ? 'enabled' : 'disabled');
    }

    function connectionStateChanged() {
        var state = websocket ? websocket.readyState : ReadyState.CLOSED;

        $('#disconnect').toggle(state == ReadyState.OPEN);
        $('#connect').toggle(state == ReadyState.CLOSED);
        $('#new-request').prop("readonly", state != ReadyState.OPEN);
        $('#server, #port').prop("readonly", state != ReadyState.CLOSED);
    }

    function initElements() {
        $('#connect').on('click', connect);
        $('#disconnect').on('click', disconnect);
        $('#new-request').autosize().on('keypress', onKeyPress);

        connectionStateChanged();
    }

    function clearMessages() {
        $('#messages ul').find('li:not(.input)').remove();
    }

    function getClass(type) {
        switch(type) {
            case MessageType.ERROR:
                return 'error';
            case MessageType.INFO:
                return 'info';
            case MessageType.RESPONSE:
                return 'received';
            case MessageType.REQUEST:
                return 'requested';
        }
    }

    function getIconClass(type) {
        switch(type) {
            case MessageType.ERROR:
                return 'fa-li fa fa-exclamation-circle';
            case MessageType.INFO:
                return 'fa-li fa fa-circle';
            case MessageType.RESPONSE:
                return 'fa-li fa fa-arrow-circle-left';
            case MessageType.REQUEST:
                return 'fa-li fa fa-arrow-circle-right';
        }
    }

    function addMessage(type, message) {
        $('<li></li>')
            .addClass(getClass(type))
            .append($('<i></i>').addClass(getIconClass(type)), message)
            .insertBefore('#messages .input');
    }

    function errorMessage(message) {
        addMessage(MessageType.ERROR, message);
    }

    function infoMessage(message) {
        addMessage(MessageType.INFO, message);
    }

    function responseMessage(message) {
        addMessage(MessageType.RESPONSE, message);
    }

    function requestMessage(message) {
        addMessage(MessageType.REQUEST, message);
    }

    function connect() {
        var host = $('#server').val();
        var port = $('#port').val()

        clearMessages();
        infoMessage("Trying " + host + ":" + port);
        $('#new-request').focus();

        var wsTarget = "ws://" + window.location.host + "/websocket"
                                                      + '?host=' + encodeURI(host)
                                                      + "&port=" + encodeURI(port);
        websocket = new WebSocket(wsTarget);
        websocket.onopen = function(evt) { onOpen(evt) };
        websocket.onclose = function(evt) { onClose(evt) };
        websocket.onmessage = function(evt) { onMessage(evt) };
        websocket.onerror = function(evt) { onError(evt) };
    }

    function disconnect() {
        websocket.close();
    }

    function send() {
        var text = $("#new-request").val();
        $("#new-request").val("");
        if (websocket.readyState == ReadyState.OPEN) {
            requestMessage(text);
            websocket.send(text + "\r\n");
        } else {
            errorMessage('websocket is not connected');
            connectionStateChanged();
        }
    }

    function onKeyPress(event) {
        if (event.keyCode == 13) {
            event.preventDefault();
            send();
        }
    }

    function onOpen(event) {
        connectionStateChanged();
    }

    function onClose(event) {
        infoMessage('Closed connection [Code: ' + event.code + '] ' + event.reason);
        connectionStateChanged();
    }

    function onMessage(event) {
        responseMessage(event.data);
    }

    function onError(event) {
        errorMessage("ERROR: " + event.data);
        connectionStateChanged();
    }
});
