# Home Manager module running the spesa Telegram bot as a user service.
# The bot's /config edits are saved in ~/.local/state/spesa/config.txt.
{ config, lib, ... }:
let
  cfg = config.services.spesa;
in
{
  options.services.spesa = {
    enable = lib.mkEnableOption "the spesa Telegram bot";

    tokenFile = lib.mkOption {
      # a string, not a path: a path literal would copy the token into the world-readable Nix store
      type = lib.types.str;
      example = "/run/secrets/spesa-telegram-token";
      description = "File containing the Telegram bot token, e.g. a sops secret.";
    };

    users = lib.mkOption {
      type = lib.types.nonEmptyListOf lib.types.int;
      example = [ 524811522 ];
      description = "Telegram user ids the bot answers; everyone else is ignored.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.spesa = {
      Unit.Description = "spesa Telegram bot";
      Service = {
        ExecStart = lib.getExe (import ./package.nix { }); # built with spesa's own pinned nixpkgs
        Environment = [
          "TELEGRAM_TOKEN_FILE=${cfg.tokenFile}"
          "SPESA_USERS=${lib.concatMapStringsSep "," toString cfg.users}"
        ];
        StateDirectory = "spesa";
        # also covers the network not being up yet at login
        Restart = "always";
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
